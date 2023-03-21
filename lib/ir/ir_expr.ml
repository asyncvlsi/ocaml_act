open! Core
module Tag = Ir_expr_tag
module K = Ir_expr0

type 'a t = { k : Ir_var.t K.t; tag : 'a Tag.t; max_bits : int }
[@@deriving sexp_of]

module U = struct
  type nonrec t = Any.t t [@@deriving sexp_of]
end

let max_layout t = Ir_layout.Bits_fixed t.max_bits
let cint_tag = Ir_expr_tag.cint_expr_tag
let cbool_tag = Ir_expr_tag.cbool_expr_tag
let untype t = Obj.magic t
