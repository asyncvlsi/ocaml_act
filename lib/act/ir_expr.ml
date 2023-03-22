open! Core
module Tag = Expr_tag
module K = Act_ir.Expr

type 'a t = { k : Act_ir.Var.t K.t; tag : 'a Tag.t; max_bits : int }
[@@deriving sexp_of]

module U = struct
  type nonrec t = Act_ir.Utils.Any.t t [@@deriving sexp_of]
end

let max_layout t = Act_ir.Layout.Bits_fixed t.max_bits
let cint_tag = Expr_tag.cint_expr_tag
let cbool_tag = Expr_tag.cbool_expr_tag
let untype t = Obj.magic t
