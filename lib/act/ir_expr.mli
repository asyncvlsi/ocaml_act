open! Core
module Tag = Expr_tag

val cbool_tag : Cbool0.t Expr_tag.t
val cint_tag : Cint0.t Expr_tag.t

module K = Ir_expr0

type 'a t = { k : Ir_var.t K.t; tag : 'a Tag.t; max_bits : int }
[@@deriving sexp_of]

module U : sig
  type nonrec t = Any.t t [@@deriving sexp_of]
end

val max_layout : 'a t -> Ir_layout.t
val untype : 'a t -> Any.t t
