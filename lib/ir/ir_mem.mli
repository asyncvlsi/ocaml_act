open! Core
module Id : Identifiable

module D : sig
  type t = {
    dtype : Any.t Ir_dtype.t;
    creation_code_pos : Code_pos.t;
    init : Any.t array;
    kind : [ `Mem | `Rom ];
  }
end

type t = {
  id : Id.t;
  d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
}

include Comparable_and_hashable.S with type t := t

val create : 'a Ir_dtype.t -> Code_pos.t -> 'a array -> [ `Mem | `Rom ] -> t
