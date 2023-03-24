open! Core
open Utils
module Id : Identifiable

type t = {
  id : Id.t;
  creation_code_pos : Utils.Code_pos.t;
  init : CInt.t option;
  bitwidth : int;
}
[@@deriving sexp_of]
(** @canonical Act_ir.Ir.Var.t *)

include Comparable with type t := t
include Hashable with type t := t

val create : int -> Utils.Code_pos.t -> CInt.t option -> t
