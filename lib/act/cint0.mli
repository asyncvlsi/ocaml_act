open! Core

type t [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val to_int_exn : t -> int
val of_int : int -> t
val bitwidth : t -> int
val zero : t
val one : t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( % ) : t -> t -> t
val shift_left : t -> t -> t
val shift_right_logical : t -> t -> t
val bit_and : t -> t -> t
val bit_or : t -> t -> t
val bit_xor : t -> t -> t
val pow : t -> t -> t
val clip : t -> bits:int -> t
val sub_wrap : t -> t -> bits:int -> t
