open! Core

type 'a t [@@deriving sexp_of]

val equal : 'a t -> 'a t -> bool

val create :
  cint_of_value:('a -> Cint0.t) -> value_of_cint:(Cint0.t -> 'a option) -> 'a t

val cint_of_value : 'a t -> 'a -> Cint0.t
val value_of_cint : 'a t -> Cint0.t -> 'a option
val cbool_expr_tag : Cbool0.t t
val cint_expr_tag : Cint0.t t
val untyped_tag : 'a t
