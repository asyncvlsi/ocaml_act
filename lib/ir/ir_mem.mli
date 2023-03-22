open! Core
module Id : Identifiable

type t = {
  id : Id.t;
  cell_bitwidth : int;
  creation_code_pos : Code_pos.t;
  init : (Cint0.t array[@hash.ignore] [@compare.ignore] [@equal.ignore]);
  kind : [ `Mem | `Rom ];
}

include Comparable_and_hashable.S with type t := t

val create : 'a Ir_dtype.t -> Code_pos.t -> 'a array -> [ `Mem | `Rom ] -> t
