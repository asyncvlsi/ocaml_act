open! Core

type t = Cbool0.t
(** @canonical Act.CBool.t *)

include Sexpable with type t := t
include Comparable with type t := t
include Hashable with type t := t

val dtype : t Dtype.t
val bitwidth : t -> int
val to_int : t -> Act_ir.CInt.t
val of_int : Act_ir.CInt.t -> t option

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

module E : sig
  type t = Cbool0.t Expr.t

  val var : Cbool0.t Var.t -> t
  val const : Cbool0.t -> t
  val to_int : t -> Act_ir.CInt.t Expr.t
  val of_int : Act_ir.CInt.t Expr.t -> t
  val true_ : t
  val false_ : t
  val eq : t -> t -> t
  val ne : t -> t -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val xor_ : t -> t -> t
  val not_ : t -> t
end

module Chp : sig
  type t = Chp.t

  val toggle : Cbool0.t Var.t -> t
  val set_false : Cbool0.t Var.t -> t
  val set_true : Cbool0.t Var.t -> t
  val match_ : E.t -> f:(Cbool0.t -> t) -> t
end
