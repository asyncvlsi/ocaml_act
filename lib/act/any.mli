open! Core

type t [@@deriving sexp]

val of_int : int -> t
val of_string : string -> t
val of_bool : bool -> t
val of_magic : 'a -> t
val to_int : t -> int
val to_string : t -> string
val to_bool : t -> bool
val to_magic : t -> 'a
