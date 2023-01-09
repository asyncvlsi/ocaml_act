open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

type 'a t

val int_ : int t
val bool_ : bool t
val string_ : string t
val of_module : (module DTypeable with type t = 'a) -> 'a t
val equal_ : 'a t -> 'a -> 'a -> bool
val sexp_of_t_ : 'a t -> 'a -> Sexp.t
val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
