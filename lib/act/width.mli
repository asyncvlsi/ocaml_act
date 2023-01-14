open! Core

type t = Fixed of int | Unlimited [@@deriving sexp]

val max : t -> t -> t
val min : t -> t -> t
val one : t
val ( + ) : t -> t -> t
