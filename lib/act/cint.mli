open! Core

type t = Cint0.t [@@deriving sexp, hash, compare, equal]

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

(* dtypes *)
val dtype_8 : t Dtype.t
val dtype_16 : t Dtype.t
val dtype_32 : t Dtype.t
val dtype_64 : t Dtype.t
val dtype : bits:int -> t Dtype.t

module E : sig
  type t = Cint0.t Expr.t

  val var : Cint0.t Var.t -> t
  val of_cint : Cint0.t -> t
  val of_int : int -> t
  val of_bool : Cbool0.t Expr.t -> t

  (* ops *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val mod_ : t -> t -> t
  val pow : t -> t -> t

  (* wrapping math *)
  val add_wrap : t -> t -> bits:int -> t
  val sub_wrap : t -> t -> bits:int -> t
  val mul_wrap : t -> t -> bits:int -> t

  (* bit math *)
  val left_shift : t -> amt:t -> t
  val right_shift : t -> amt:t -> t
  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> bits:int -> t
  val bit_at : t -> bit:int -> t
  val bit_slice : t -> lower_inc:int -> upper_inc:int -> t

  (* compairison *)
  val eq : t -> t -> Cbool0.t Expr.t
  val ne : t -> t -> Cbool0.t Expr.t
  val lt : t -> t -> Cbool0.t Expr.t
  val gt : t -> t -> Cbool0.t Expr.t
  val le : t -> t -> Cbool0.t Expr.t
  val ge : t -> t -> Cbool0.t Expr.t
  val is_zero : t -> Cbool0.t Expr.t
  val is_nonzero : t -> Cbool0.t Expr.t

  (* clippers *)
  val clip : t -> bits:int -> t
  val assert_fits : t -> bits:int -> t

  (* concatination *)
  val concat2 : t * [ `Bits of int ] -> t * [ `Bits of int ] -> t
  val concat : (t * [ `Bits of int ]) list -> t

  (* small constants *)
  val zero : t
  val one : t
  val two : t
  val three : t
  val four : t
  val five : t
end

module Chp : sig
  type t = Chp.t

  val assign :
    Cint0.t Var.t -> Cint0.t Expr.t -> overflow:Overflow_behavior.t -> t

  val incr : Cint0.t Var.t -> overflow:Overflow_behavior.t -> t
  val decr : Cint0.t Var.t -> underflow:Overflow_behavior.t -> t

  val send :
    Cint0.t Chan.W.t -> Cint0.t Expr.t -> overflow:Overflow_behavior.t -> t

  val send' :
    Cint0.t Chan.W.t -> Cint0.t Var.t -> overflow:Overflow_behavior.t -> t
end

module Chan : sig
  val bw_le : Cint0.t Chan.R.t -> int -> bool
  val bw_ge : Cint0.t Chan.W.t -> int -> bool
end
