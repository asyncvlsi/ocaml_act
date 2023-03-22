open! Core

module type E_S = sig
  type elt
  type t = elt Expr.t

  val var : elt Var.t -> elt Expr.t
  val const : elt -> elt Expr.t
  val eq : elt Expr.t -> elt Expr.t -> Cbool0.t Expr.t
  val to_int : elt Expr.t -> Act_ir.CInt.t Expr.t
  val of_int : Act_ir.CInt.t Expr.t -> elt Expr.t
end

module type Chp_S = sig
  type elt
  type t = Chp.t

  val match_ : elt Expr.t -> f:(elt -> t) -> t
end

module type S = sig
  type t

  include Sexpable with type t := t
  include Comparable with type t := t
  include Hashable with type t := t

  val dtype : t Dtype.t
  val bitwidth : t -> int
  val to_int : t -> Act_ir.CInt.t
  val of_int : Act_ir.CInt.t -> t option

  module E : E_S with type elt := t
  module Chp : Chp_S with type elt := t
end

module Make (X : sig
  type t [@@deriving sexp, hash, compare, equal]

  val mapping : (t * Act_ir.CInt.t) list
end) : S with type t := X.t = struct
  module T = struct
    type t = X.t [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_int t = List.find_exn X.mapping ~f:(fun (op, _) -> X.equal op t) |> snd

  let of_int i =
    List.find X.mapping ~f:(fun (_, op_code) -> Act_ir.CInt.equal op_code i)
    |> Option.map ~f:fst

  let bitwidth t = to_int t |> Act_ir.CInt.bitwidth

  let max_bitwidth =
    List.map X.mapping ~f:(fun (t, _) -> bitwidth t)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn

  let expr_tag = Expr_tag.create ~cint_of_value:to_int ~value_of_cint:of_int

  let ok_cint_intervals =
    let l =
      List.map X.mapping ~f:snd |> Act_ir.CInt.Set.of_list |> Core.Set.to_list
      |> Array.of_list
    in
    Array.filter_mapi l ~f:(fun i v ->
        if Int.(i > 0) && Act_ir.CInt.equal l.(i - 1) Act_ir.CInt.(sub v one)
        then None
        else
          let n = ref 1 in
          let j = ref (i + 1) in
          while
            Int.(!j < Array.length l)
            && Act_ir.CInt.(eq l.(!j) (add v (of_int !n)))
          do
            incr j;
            incr n
          done;
          Some (v, !n))
    |> Array.to_list

  let of_cint_assert_expr_fn =
    List.map ok_cint_intervals ~f:(fun (v, n) ->
        match n with
        | 1 -> Act_ir.Ir.Expr.(Eq (Const v, Var ()))
        | _ ->
            if Act_ir.CInt.equal v (Act_ir.CInt.of_int 0) then
              Act_ir.Ir.Expr.(Lt (Var (), Const Act_ir.CInt.(add v (of_int n))))
            else
              Act_ir.Ir.Expr.(
                BitAnd
                  ( Le (Const v, Var ()),
                    Lt (Var (), Const Act_ir.CInt.(add v (of_int n))) )))
    |> List.reduce ~f:(fun a b -> Act_ir.Ir.Expr.(BitOr (a, b)))
    |> Option.value_exn

  let dtype =
    Ir_dtype.create ~equal ~sexp_of_t:X.sexp_of_t
      ~max_layout_of:(fun t -> Bits_fixed (bitwidth t))
      ~cint_of:(fun t -> to_int t)
      ~of_cint:of_int ~layout:(Bits_fixed max_bitwidth) ~of_cint_assert_expr_fn
      ~expr_tag
    |> Dtype.Internal.wrap

  module E = struct
    type t = X.t Expr.t

    let tag = expr_tag
    let var v = Expr.var v

    let expr_of_int_expr i =
      let assert_e =
        List.map ok_cint_intervals ~f:(fun (v, n) ->
            match n with
            | 1 -> Expr.(eq (of_cint v) i)
            | _ ->
                if Act_ir.CInt.equal v (Act_ir.CInt.of_int 0) then
                  Expr.(lt i (of_cint Act_ir.CInt.(add v (of_int n))))
                else
                  Cbool.E.and_
                    Expr.(le (of_cint v) i)
                    Expr.(lt i (of_cint Act_ir.CInt.(add v (of_int n)))))
        |> List.reduce ~f:Cbool.E.or_ |> Option.value_exn
      in

      let e =
        Expr.with_assert_log ~assert_e ~val_e:i ~log_e:i (fun i ->
            [%string "of_int for invalid enum value %{i#Act_ir.CInt}"])
      in
      Expr.Internal.with_set_tag_and_max_bits e tag max_bitwidth

    let expr_to_int_expr t =
      assert (Expr_tag.equal (Expr.Internal.tag t) tag);
      Expr.Internal.with_set_tag_and_max_bits t Expr_tag.cint_expr_tag
        (Expr.Internal.max_bits t)

    let const c =
      Expr.Internal.with_set_tag_and_max_bits
        (to_int c |> Expr.of_cint)
        tag max_bitwidth

    let eq a b = Expr.eq (expr_to_int_expr a) (expr_to_int_expr b)
    let of_int = expr_of_int_expr
    let to_int = expr_to_int_expr
  end

  module Chp = struct
    type t = Chp.t

    let match_ expr ~f =
      Chp.select_imm ~else_:None
        (List.map X.mapping ~f:(fun (op, _) ->
             let guard = E.(eq (const op) expr) in
             (guard, f op)))
  end
end
