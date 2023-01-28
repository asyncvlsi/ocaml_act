open! Core

module type E_S = sig
  type elt
  type t = elt Expr.Wrap.t

  val var : elt Var.Wrap.t -> elt Expr.Wrap.t
  val const : elt -> elt Expr.Wrap.t
  val eq : elt Expr.Wrap.t -> elt Expr.Wrap.t -> Cbool0.t Expr.Wrap.t
  val to_int : elt Expr.Wrap.t -> Cint0.t Expr.Wrap.t
  val of_int : Cint0.t Expr.Wrap.t -> elt Expr.Wrap.t
end

module type N_S = sig
  type elt
  type t = Node.Wrap.t

  val match_ : elt Expr.Wrap.t -> f:(elt -> t) -> t
end

module type S = sig
  type t

  include Sexpable with type t := t
  include Comparable with type t := t
  include Hashable with type t := t

  val dtype : t Dtype.Wrap.t
  val bitwidth : t -> int
  val to_int : t -> Cint0.t
  val of_int : Cint0.t -> t option

  module E : E_S with type elt := t
  module N : N_S with type elt := t
end

module Make (X : sig
  type t [@@deriving sexp, hash, compare, equal]

  val mapping : (t * Cint0.t) list
end) : S with type t := X.t = struct
  module T = struct
    type t = X.t [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_int t = List.find_exn X.mapping ~f:(fun (op, _) -> X.equal op t) |> snd

  let of_int i =
    List.find X.mapping ~f:(fun (_, op_code) -> Cint0.equal op_code i)
    |> Option.map ~f:fst

  let bitwidth t = to_int t |> Cint0.bitwidth

  let max_bitwidth =
    List.map X.mapping ~f:(fun (t, _) -> bitwidth t)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn

  let expr_tag = Expr_tag.create ~cint_of_value:to_int ~value_of_cint:of_int

  let dtype =
    Dtype.Wrap.create ~equal ~sexp_of_t:X.sexp_of_t
      ~max_layout_of:(fun t -> Bits_fixed (bitwidth t))
      ~cint_of:(fun t -> to_int t)
      ~of_cint:of_int ~layout:(Bits_fixed max_bitwidth) ~expr_tag

  module E = struct
    type t = X.t Expr.Wrap.t

    let tag = expr_tag
    let var v = Expr.Wrap.var v

    let expr_of_int_expr i =
      let assert_e =
        let l =
          List.map X.mapping ~f:snd |> Cint0.Set.of_list |> Core.Set.to_list
          |> Array.of_list
        in
        let l =
          Array.filter_mapi l ~f:(fun i v ->
              if Int.(i > 0) && Cint0.equal l.(i - 1) Cint0.(v - of_int 1) then
                None
              else
                let n = ref 1 in
                let j = ref (i + 1) in
                while
                  Int.(!j < Array.length l)
                  && Cint0.equal l.(!j) Cint0.(v + of_int !n)
                do
                  incr j;
                  incr n
                done;
                Some (v, !n))
          |> Array.to_list
        in
        List.map l ~f:(fun (v, n) ->
            match n with
            | 1 -> Expr.CInt.(eq (const v) i)
            | _ ->
                if Cint0.equal v (Cint0.of_int 0) then
                  Expr.CInt.(lt i (const Cint0.(v + of_int n)))
                else
                  Cbool.E.and_
                    Expr.CInt.(le (const v) i)
                    Expr.CInt.(lt i (const Cint0.(v + of_int n))))
        |> List.reduce ~f:Cbool.E.or_ |> Option.value_exn
      in
      let i =
        Expr.with_assert_log ~assert_e ~val_e:i ~log_e:i (fun i ->
            [%string "of_int for invalid enum value %{i#Cint0}"])
        |> Expr.Ir.unwrap
      in
      Expr.Ir.wrap
        { Expr.Ir.k = i.k; tag; max_bits = Int.min max_bitwidth i.max_bits }

    let expr_to_int_expr t =
      let t = Expr.Ir.unwrap t in
      assert (Expr_tag.equal t.tag tag);
      Expr.Ir.wrap
        { Expr.Ir.k = t.k; tag = Expr.CInt.tag; max_bits = t.max_bits }

    let const c = to_int c |> Expr.CInt.const |> expr_of_int_expr
    let eq a b = Expr.CInt.eq (expr_to_int_expr a) (expr_to_int_expr b)
    let of_int = expr_of_int_expr
    let to_int = expr_to_int_expr
  end

  module N = struct
    type t = Node.Wrap.t

    let match_ expr ~f =
      Node.Wrap.select_imm ~else_:None
        (List.map X.mapping ~f:(fun (op, _) ->
             let guard = E.(eq (const op) expr) in
             (guard, f op)))
  end
end
