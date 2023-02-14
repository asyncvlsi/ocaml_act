open! Core

type t = Cint0.t [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

(* math *)
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

(* dtypes *)
val dtype_8 : t Dtype.Wrap.t
val dtype_16 : t Dtype.Wrap.t
val dtype_32 : t Dtype.Wrap.t
val dtype_64 : t Dtype.Wrap.t
val dtype : bits:int -> t Dtype.Wrap.t

module E : sig
  type t = Cint0.t Expr.Wrap.t

  val var : Cint0.t Var.Wrap.t -> t
  val const : Cint0.t -> t
  val cint : int -> t
  val of_bool : Cbool0.t Expr.Wrap.t -> t

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
  val lshift : t -> amt:t -> t
  val rshift : t -> amt:t -> t
  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> bits:int -> t
  val bit_at : t -> bit:int -> t
  val bit_slice : t -> lower_inc:int -> upper_inc:int -> t

  (* compairison *)
  val eq : t -> t -> Cbool0.t Expr.Wrap.t
  val ne : t -> t -> Cbool0.t Expr.Wrap.t
  val lt : t -> t -> Cbool0.t Expr.Wrap.t
  val gt : t -> t -> Cbool0.t Expr.Wrap.t
  val le : t -> t -> Cbool0.t Expr.Wrap.t
  val ge : t -> t -> Cbool0.t Expr.Wrap.t
  val is_zero : t -> Cbool0.t Expr.Wrap.t
  val is_nonzero : t -> Cbool0.t Expr.Wrap.t

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
  type t = Chp_node.Wrap.t

  val assign :
    ?loc:Code_pos.t ->
    Cint0.t Var.Wrap.t ->
    Cint0.t Expr.Wrap.t ->
    overflow:Overflow_behavior.t ->
    t

  val incr :
    ?loc:Code_pos.t -> Cint0.t Var.Wrap.t -> overflow:Overflow_behavior.t -> t

  val decr :
    ?loc:Code_pos.t -> Cint0.t Var.Wrap.t -> underflow:Overflow_behavior.t -> t

  val send :
    ?loc:Code_pos.t ->
    Cint0.t Chan.Wrap.W.t ->
    Cint0.t Expr.Wrap.t ->
    overflow:Overflow_behavior.t ->
    t

  val send' :
    ?loc:Code_pos.t ->
    Cint0.t Chan.Wrap.W.t ->
    Cint0.t Var.Wrap.t ->
    overflow:Overflow_behavior.t ->
    t
end
