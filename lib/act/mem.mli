open! Core

(* It is not allowed to have operation on the memory from two different locations
   in the program simultaniously. *)

module Wrap : sig
  type ('kind, 'a) t [@@deriving sexp_of]
  type 'a ug_mem = ([ `Mem ], 'a) t
  type 'a ug_rom = ([ `Rom ], 'a) t

  val create_ug_mem :
    ?loc:Code_pos.t -> 'a Dtype.Wrap.t -> 'a array -> 'a ug_mem

  val create_ug_rom :
    ?loc:Code_pos.t -> 'a Dtype.Wrap.t -> 'a array -> 'a ug_rom
end

(* The internal data structures. These are only meant to be constructed throguh the above interfaces. *)
module Ir : sig
  module Id : Identifiable

  module D : sig
    type t = {
      dtype : Any.t Dtype.Ir.t;
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

  val unwrap_ug_mem : 'a Wrap.ug_mem -> t
  val unwrap_ug_rom : 'a Wrap.ug_rom -> t
end
