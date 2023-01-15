open! Core

type t = Cbool0.t [@@deriving sexp, hash, compare, equal]

include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

val dtype : t Dtype.Wrap.t
val true_ : t
val false_ : t
val of_bool : bool -> t
val bitwidth : t -> int

module E : sig
  type t = Cbool0.t Expr.Wrap.t

  val var : Cbool0.t Var.Wrap.t -> t
  val true_ : t
  val false_ : t
  val not_ : t -> t
end

module N : sig
  type t = Node.Wrap.t

  val toggle : ?loc:Code_pos.t -> Cbool0.t Var.Wrap.t -> t
end
