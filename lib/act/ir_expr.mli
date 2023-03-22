open! Core
module Tag = Expr_tag

val cbool_tag : Act_ir.CBool.t Expr_tag.t
val cint_tag : Act_ir.CInt.t Expr_tag.t

module K = Act_ir.Expr

type 'a t = { k : Act_ir.Var.t K.t; tag : 'a Tag.t; max_bits : int }
[@@deriving sexp_of]

module U : sig
  type nonrec t = Act_ir.Utils.Any.t t [@@deriving sexp_of]
end

val max_layout : 'a t -> Act_ir.Layout.t
val untype : 'a t -> Act_ir.Utils.Any.t t
