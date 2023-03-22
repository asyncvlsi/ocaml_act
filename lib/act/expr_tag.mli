open! Core

type 'a t [@@deriving sexp_of]

val equal : 'a t -> 'a t -> bool

val create :
  cint_of_value:('a -> Act_ir.CInt.t) ->
  value_of_cint:(Act_ir.CInt.t -> 'a option) ->
  'a t

val cint_of_value : 'a t -> 'a -> Act_ir.CInt.t
val value_of_cint : 'a t -> Act_ir.CInt.t -> 'a option
val cbool_expr_tag : Cbool0.t t
val cint_expr_tag : Act_ir.CInt.t t
