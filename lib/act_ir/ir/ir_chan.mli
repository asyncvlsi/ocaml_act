open! Core
module Id : Identifiable

type t = { id : Id.t; bitwidth : int; creation_code_pos : Utils.Code_pos.t }
[@@deriving sexp, equal, compare, hash]

include Comparable with type t := t
include Hashable with type t := t

val create : int -> Utils.Code_pos.t -> t
