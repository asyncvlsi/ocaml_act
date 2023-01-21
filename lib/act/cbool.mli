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

module N : sig
  include Enum.N_S with type elt := t

  type t = Node.Wrap.t

  val toggle : ?loc:Code_pos.t -> Cbool0.t Var.Wrap.t -> t
  val set_false : ?loc:Code_pos.t -> Cbool0.t Var.Wrap.t -> t
  val set_true : ?loc:Code_pos.t -> Cbool0.t Var.Wrap.t -> t
end
