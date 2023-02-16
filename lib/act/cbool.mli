open! Core
include Enum.S with type t = Cbool0.t
include Comparable with type t := t
include Hashable with type t := t
include Stringable with type t := t

module E : sig
  include Enum.E_S with type elt := t

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
  include Enum.Chp_S with type elt := t

  type t = Chp.t

  val toggle : Cbool0.t Var.t -> t
  val set_false : Cbool0.t Var.t -> t
  val set_true : Cbool0.t Var.t -> t
end
