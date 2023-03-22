open! Core

type t [@@deriving sexp, hash, compare, equal]
(** @canonical Act_ir.CInt.t *)

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val to_int_exn : t -> int
val of_int : int -> t
val bitwidth : t -> int
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val mod_ : t -> t -> t
val left_shift : t -> amt:t -> t
val right_shift : t -> amt:t -> t
val left_shift' : t -> amt:int -> t
val right_shift' : t -> amt:int -> t
val bit_and : t -> t -> t
val bit_or : t -> t -> t
val bit_xor : t -> t -> t
val pow : t -> t -> t
val eq : t -> t -> bool
val ne : t -> t -> bool
val lt : t -> t -> bool
val le : t -> t -> bool
val gt : t -> t -> bool
val ge : t -> t -> bool
val clip : t -> bits:int -> t
val add_wrap : t -> t -> bits:int -> t
val sub_wrap : t -> t -> bits:int -> t
val zero : t
val one : t
val two : t
val three : t
val four : t
val five : t
val ones_mask : lower_inc:int -> upper_inc:int -> t
val bit_slice : t -> lower_inc:int -> upper_inc:int -> t
