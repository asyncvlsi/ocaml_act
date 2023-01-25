open! Core

type t = bool [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val of_bool : bool -> t
val bitwidth : t -> int
val expr_tag : bool Expr_tag.t
val to_cint : t -> Cint0.t
val of_cint : Cint0.t -> t option
