open! Core

type 'a t [@@deriving sexp_of]

module Ir : sig
  type 'a outer = 'a t
  type 'a t [@@deriving sexp_of]

  val create :
    equal:('a -> 'a -> bool) ->
    sexp_of_t:('a -> Sexp.t) ->
    max_layout_of:('a -> Layout.t) ->
    cint_of:('a -> Cint0.t) ->
    of_cint:(Cint0.t -> 'a option) ->
    of_cint_assert_expr_fn:unit Expr0.t ->
    layout:Layout.t ->
    expr_tag:'a Expr_tag.t ->
    'a outer

  val dummy_val : Any.t t
  val wrap : 'a t -> 'a outer
  val unwrap : 'a outer -> 'a t
  val untype : 'a t -> Any.t t
  val untype' : 'a outer -> Any.t t
  val equal_ : 'a t -> 'a -> 'a -> bool
  val sexp_of_t_ : 'a t -> 'a -> Sexp.t
  val cint_of_value : 'a t -> 'a -> Cint0.t
  val of_cint_assert_expr_fn : 'a t -> unit Expr0.t
  val value_of_cint_exn : 'a t -> Cint0.t -> 'a
  val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
  val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
  val max_layout_of : 'a t -> 'a -> Layout.t
  val layout : 'a t -> Layout.t
  val fits_into_dtype : into:'a t -> Layout.t -> bool
  val fits_value : 'a t -> 'a -> bool
  val expr_tag : 'a t -> 'a Expr_tag.t
end
