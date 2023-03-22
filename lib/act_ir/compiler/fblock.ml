open! Core
module Ins_idx = Int

module U = struct
  type 'v t = { ins : 'v list; es : (* dst *) ('v * Ins_idx.t F_expr.t) list }
  [@@deriving sexp_of]
end

type ('v, 'vc) t = {
  ins : 'v list;
  es : (* dst *) ('v * Ins_idx.t F_expr.t) list;
}

type ('v, 'vc) outer = ('v, 'vc) t

module type S = sig
  type var
  type comparator_witness
  type t = (var, comparator_witness) outer [@@deriving sexp_of]

  val create1 : var -> var F_expr.t -> t
  val expr_list : t -> (var * var F_expr.t) list
  val ins : t -> var list
  val outs : t -> var list
  val deps_of_outs : t -> (var * var list) list
  val filter_outs : t -> f:(var -> bool) -> t
  val map_ins : t -> f:(var -> var) -> t
  val dup_ids : t -> var list list
  val merge_same_reads : t -> t -> t
  val append : t -> t -> t
  val get_consts_if_const : t -> (var * Cint.t) list option

  val map_ins_and_outs :
    ('v, 'vc) outer -> map_in:('v -> var) -> map_out:('v -> var) -> t
end

let eval_const_expr e =
  let rec f e =
    match e with
    | F_expr.Var _ -> failwith "Expr is not const"
    | Const c -> c
    | Add (a, b) -> Cint.add (f a) (f b)
    | Sub_no_wrap (a, b) -> Cint.sub (f a) (f b)
    | Mul (a, b) -> Cint.mul (f a) (f b)
    | Div (a, b) -> Cint.div (f a) (f b)
    | Mod (a, b) -> Cint.mod_ (f a) (f b)
    | LShift (a, b) -> Cint.left_shift (f a) ~amt:(f b)
    | RShift (a, b) -> Cint.right_shift (f a) ~amt:(f b)
    | BitAnd (a, b) -> Cint.bit_and (f a) (f b)
    | BitOr (a, b) -> Cint.bit_or (f a) (f b)
    | BitXor (a, b) -> Cint.bit_xor (f a) (f b)
    | Clip (e, bits) -> Cint.clip (f e) ~bits
    | Concat es ->
        let _, c =
          List.fold es ~init:(0, Cint.zero) ~f:(fun (acc, tot) (e, bits) ->
              ( acc + bits,
                Cint.add tot (Cint.left_shift (f e) ~amt:(Cint.of_int acc)) ))
        in
        c
    | Log2OneHot _ -> failwith "TODO"
    | Eq (a, b) -> Cint.eq (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Ne (a, b) -> Cint.ne (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Lt (a, b) -> Cint.lt (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Le (a, b) -> Cint.le (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Gt (a, b) -> Cint.gt (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Ge (a, b) -> Cint.ge (f a) (f b) |> Bool.to_int |> Cint.of_int
    | Eq0 a -> Cint.eq (f a) Cint.zero |> Bool.to_int |> Cint.of_int
  in
  f e

module Make (Var : sig
  type t [@@deriving sexp, hash, compare, equal]
  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.comparator
end) :
  S with type var = Var.t and type comparator_witness = Var.comparator_witness =
struct
  type nonrec t = (Var.t, Var.comparator_witness) t

  let sexp_of_t { ins; es } = U.sexp_of_t Var.sexp_of_t { U.ins; es }

  module Var = struct
    include Var
    include Comparable.Make_using_comparator (Var)
  end

  type var = Var.t
  type comparator_witness = Var.comparator_witness

  let _f (_ : var) (_ : t) (_ : comparator_witness) = failwith "TODO"

  let of_ins_and_es ~ins es =
    let ins = Set.to_list ins in
    let idx_of_in =
      List.mapi ins ~f:(fun i v -> (v, i)) |> Var.Map.of_alist_exn
    in
    let es =
      List.map es ~f:(fun (dst, e) ->
          (dst, F_expr.map_vars e ~f:(Map.find_exn idx_of_in)))
    in
    { ins; es }

  let of_assigns l =
    let ins =
      List.concat_map l ~f:(fun (_, e) -> F_expr.var_ids e) |> Var.Set.of_list
    in
    of_ins_and_es ~ins l

  let expr_list fblock =
    List.map fblock.es ~f:(fun (dst, e) ->
        let e = F_expr.map_vars e ~f:(fun i -> List.nth_exn fblock.ins i) in
        (dst, e))

  let create1 dst e = of_assigns [ (dst, e) ]
  let ins fblock = fblock.ins
  let outs fblock = List.map fblock.es ~f:fst

  let deps_of_outs fblock =
    List.map fblock.es ~f:(fun (dst, e) ->
        ( dst,
          F_expr.var_ids e |> Int.Set.of_list
          |> Var.Set.map ~f:(fun i -> List.nth_exn fblock.ins i)
          |> Set.to_list ))

  let filter_outs fblock ~f =
    let es = List.filter fblock.es ~f:(fun (dst, _) -> f dst) in
    { ins = fblock.ins; es }

  let map_ins fblock ~f =
    let ins = List.map fblock.ins ~f in
    let var_of_old_id =
      List.mapi ins ~f:(fun i v -> (i, v)) |> Int.Map.of_alist_exn
    in
    let ins = Var.Set.of_list ins |> Set.to_list in
    let new_id_of_var =
      List.mapi ins ~f:(fun i v -> (v, i)) |> Var.Map.of_alist_exn
    in
    let new_id_of_old_id =
      Map.map var_of_old_id ~f:(Map.find_exn new_id_of_var)
    in
    let es =
      List.map fblock.es ~f:(fun (dst, e) ->
          (dst, F_expr.map_vars e ~f:(Map.find_exn new_id_of_old_id)))
    in
    { ins; es }

  let dup_ids fblock =
    let module F_expr_map = Map.Make (struct
      type t = Int.t F_expr.t [@@deriving sexp, compare, equal, hash]
    end) in
    List.map fblock.es ~f:(fun (dst, e) -> (e, dst))
    |> F_expr_map.of_alist_multi |> Map.data
    |> List.filter ~f:(fun l -> List.length l >= 2)

  let merge_same_reads fblock1 fblock2 =
    assert (
      Set.equal (Var.Set.of_list fblock1.ins) (Var.Set.of_list fblock2.ins));
    let es = expr_list fblock1 @ expr_list fblock2 in
    of_ins_and_es ~ins:(Var.Set.of_list fblock1.ins) es

  (* stack fblock2 below fblock1. Rewrite fblock2 so it doesnt read fblock1
     outputs *)
  let append fblock1 fblock2 =
    let es1 = expr_list fblock1 in
    let es2 = expr_list fblock2 in
    let e_of_in2, ins2 =
      List.partition_map fblock2.ins ~f:(fun in_ ->
          match List.find es1 ~f:(fun (dst, _) -> Var.equal dst in_) with
          | Some (dst, e) -> Either.First (dst, e)
          | None -> Second in_)
    in
    let e_of_in2 = Var.Map.of_alist_exn e_of_in2 in
    let es2 =
      List.map es2 ~f:(fun (dst, e) ->
          (dst, F_expr.bind_vars e ~f:(Map.find_exn e_of_in2)))
    in
    let ins = fblock1.ins @ ins2 |> Var.Set.of_list in
    let es = es1 @ es2 in
    of_ins_and_es ~ins es

  let get_consts_if_const fblock =
    if ins fblock |> List.is_empty then
      let es =
        List.map fblock.es ~f:(fun (dst, e) -> (dst, eval_const_expr e))
      in
      Some es
    else None

  let map_ins_and_outs (fblock : ('a, 'ac) outer) ~(map_in : 'a -> Var.t)
      ~(map_out : 'a -> Var.t) : t =
    let ins = List.map fblock.ins ~f:map_in in
    let es = List.map fblock.es ~f:(fun (dst, e) -> (map_out dst, e)) in
    assert (Var.Set.of_list ins |> Set.length |> Int.equal (List.length ins));
    assert (
      List.map es ~f:fst |> Var.Set.of_list |> Set.length
      |> Int.equal (List.length es));
    ({ ins; es } : t)
end
