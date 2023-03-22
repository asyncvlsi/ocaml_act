open! Core

type t = bool [@@deriving sexp, hash, compare, equal]
(** @canonical Act.CBool.t *)

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val of_bool : bool -> t
val to_bool : t -> bool
val bitwidth : t -> int
val to_cint : t -> Act_ir.CInt.t
val of_cint : Act_ir.CInt.t -> t option
val not_ : t -> t
val and_ : t -> t -> t
val or_ : t -> t -> t
val xor_ : t -> t -> t
val bool_eq : t -> t -> t
val bool_neq : t -> t -> t
