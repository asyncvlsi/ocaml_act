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
val add_wrap : t -> t -> bits:int -> t
val sub_wrap : t -> t -> bits:int -> t

(* dtypes *)
val dtype_8 : t Dtype.Wrap.t
val dtype_16 : t Dtype.Wrap.t
val dtype_32 : t Dtype.Wrap.t
val dtype_64 : t Dtype.Wrap.t
val dtype : bits:int -> t Dtype.Wrap.t

(* nodes *)
module N : sig
  type t = Node.Wrap.t

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

module E : sig
  type t = Cint0.t Expr.Wrap.t

  val var : Cint0.t Var.Wrap.t -> t
  val const : Cint0.t -> t
  val cint : int -> t

  (* ops *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val mod_ : t -> t -> t
  val lshift : t -> amt:t -> t
  val rshift : t -> amt:t -> t
  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val eq : t -> t -> Cbool0.t Expr.Wrap.t
  val ne : t -> t -> Cbool0.t Expr.Wrap.t
  val clip : t -> bits:int -> t
  val add_wrap : t -> t -> bits:int -> t
  val sub_wrap : t -> t -> bits:int -> t

  val zero: t
end
