open! Core

module Id : sig
  include Identifiable

  val of_int : int -> t
end

type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
[@@deriving sexp, hash, equal, compare]

include Comparable with type t := t
include Hashable with type t := t

val bitwidth : t -> int
