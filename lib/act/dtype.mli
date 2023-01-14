open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

type 'a t

val int32 : Cint.t t
val bool_ : bool t
val string_ : string t

module Ir : sig
  type 'a outer = 'a t
  type 'a t

  val unwrap : 'a outer -> 'a t
  val untype : 'a t -> Any.t t
  val untype' : 'a outer -> Any.t t
  val equal_ : 'a t -> 'a -> 'a -> bool
  val sexp_of_t_ : 'a t -> 'a -> Sexp.t
  val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
  val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
  val width : 'a t -> Width.t
end
