open! Core

type t [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val true_ : t
val false_ : t
val of_bool : bool -> t
val bitwidth : t -> int
