open! Core

type t [@@deriving sexp_of]

val of_int : int -> t
val of_string : string -> t
val of_bool : bool -> t
val of_magic : 'a -> t
val to_int : t -> int
val to_string : t -> string
val to_bool : t -> bool
val to_magic : t -> 'a

module Option : sig
  val of_magic : 'a option -> t option
  val to_magic : t option -> 'a option
end

module Array : sig
  val of_magic : 'a array -> t array
  val to_magic : t array -> 'a array
end
