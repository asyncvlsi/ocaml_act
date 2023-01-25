open! Core
include Cbool0
include Comparable.Make (Cbool0)
include Hashable.Make (Cbool0)

let to_int t = to_cint t
let of_int i = of_cint i
let bitwidth t = to_int t |> Cint0.bitwidth
let max_bitwidth = 1
let expr_tag = Cbool0.expr_tag

let dtype =
  Dtype.Wrap.create ~equal ~sexp_of_t
    ~max_layout_of:(fun t -> Bits_fixed (bitwidth t))
    ~cint_of:(fun t -> to_int t)
    ~of_cint:of_int ~layout:(Bits_fixed max_bitwidth) ~expr_tag

module E = struct
  type nonrec t = t Expr.Wrap.t

  let tag = expr_tag
  let var v = Expr.Wrap.var v

  let expr_of_int_expr i =
    (* TODO add assert node. TODO runtime assertions? *)
    let i = Expr.Ir.unwrap i in
    Expr.Ir.wrap
      { Expr.Ir.k = i.k; tag; max_bits = Int.min max_bitwidth i.max_bits }

  let expr_to_int_expr t =
    (* TODO add assert node. TODO runtime assertions? *)
    let t = Expr.Ir.unwrap t in
    assert (Expr_tag.equal t.tag tag);
    Expr.Ir.wrap { Expr.Ir.k = t.k; tag = Expr.CInt.tag; max_bits = t.max_bits }

  let const c = to_int c |> Expr.CInt.const |> expr_of_int_expr
  let of_int = expr_of_int_expr
  let to_int = expr_to_int_expr
  let true_ = const true
  let false_ = const false
  let not_ e = Expr.CInt.eq (to_int e) (Expr.CInt.cint 0)
  let eq a b = Expr.CInt.eq (expr_to_int_expr a) (expr_to_int_expr b)
  let ne a b = eq a b |> not_

  let and_ a b =
    Expr.CInt.bit_and (expr_to_int_expr a) (expr_to_int_expr b)
    |> expr_of_int_expr

  let or_ a b =
    Expr.CInt.bit_or (expr_to_int_expr a) (expr_to_int_expr b)
    |> expr_of_int_expr

  let xor_ a b =
    Expr.CInt.bit_xor (expr_to_int_expr a) (expr_to_int_expr b)
    |> expr_of_int_expr
end

module N = struct
  type t = Node.Wrap.t

  let match_ expr ~f =
    Node.Wrap.select_imm ~else_:None
      (List.map [ false; true ] ~f:(fun op -> (E.(eq (const op) expr), f op)))

  let toggle ?loc (var_id : Cbool0.t Var.Wrap.t) =
    Node.Wrap.assign ?loc var_id E.(var var_id |> not_)

  let set_true ?loc var_id = Node.Wrap.assign ?loc var_id E.true_
  let set_false ?loc var_id = Node.Wrap.assign ?loc var_id E.false_
end
