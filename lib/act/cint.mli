open! Core

type t [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val of_int : int -> t
val width : t -> int
val width' : t -> Width.t
