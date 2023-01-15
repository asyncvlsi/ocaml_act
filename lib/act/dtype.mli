open! Core

module Wrap : sig
  type 'a t

  val create :
    equal:('a -> 'a -> bool) ->
    sexp_of_t:('a -> Sexp.t) ->
    max_layout_of:('a -> Layout.t) ->
    layout:Layout.t ->
    'a t
end

module Ir : sig
  type 'a t

  val dummy_val : Any.t t
  val unwrap : 'a Wrap.t -> 'a t
  val untype : 'a t -> Any.t t
  val untype' : 'a Wrap.t -> Any.t t
  val equal_ : 'a t -> 'a -> 'a -> bool
  val sexp_of_t_ : 'a t -> 'a -> Sexp.t
  val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
  val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
  val max_layout_of : 'a t -> 'a -> Layout.t
  val layout : 'a t -> Layout.t
  val fits_into_dtype : into:'a t -> Layout.t -> bool
  val fits_value : 'a t -> 'a -> bool
end
