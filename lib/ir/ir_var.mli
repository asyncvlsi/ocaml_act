open! Core
module Id : Identifiable

type t = {
  id : Id.t;
  creation_code_pos : Code_pos.t;
  init : Cint0.t option;
  bitwidth : int;
}
[@@deriving sexp_of]

include Comparable with type t := t
include Hashable with type t := t

val create : int -> Code_pos.t -> Cint0.t option -> t
