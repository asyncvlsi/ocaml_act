open! Core
module Id : Identifiable

type t = {
  id : Id.t;
  cell_bitwidth : int;
  creation_code_pos : Utils.Code_pos.t;
  init : (Cint.t array[@hash.ignore] [@compare.ignore] [@equal.ignore]);
  kind : [ `Mem | `Rom ];
}
(** @canonical Act_ir.Ir.Mem.t *)

include Comparable_and_hashable.S with type t := t

val create : int -> Utils.Code_pos.t -> Cint.t array -> [ `Mem | `Rom ] -> t
