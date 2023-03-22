open! Core
module Id : Identifiable

type t = { id : Id.t; bitwidth : int; creation_code_pos : Utils.Code_pos.t }
[@@deriving sexp, equal]
(** @canonical Act_ir.Ir.Chan.t *)

type comparator_witness
(** @canonical Act_ir.Ir.Chan.comparator_witness *)

include
  Comparable with type t := t and type comparator_witness := comparator_witness

include Hashable with type t := t

val create : int -> Utils.Code_pos.t -> t
