open! Core

module Id : sig
  include Identifiable

  val create : unit -> t
end = struct
  include Int

  let next_int = ref 0

  let create () =
    let id = !next_int in
    incr next_int;
    id
end

module UnguardedMemRom_u = struct
  module Id = Id

  module D = struct
    type t = {
      dtype : Any.t Dtype.Ir.t;
      creation_code_pos : Code_pos.t;
      init : Any.t array;
      kind : [ `Mem | `Rom ];
    }
  end

  module T = struct
    type t = {
      id : Id.t;
      d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
    }
    [@@deriving hash, compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create dtype init creation_code_pos kind =
    let id = Id.create () in
    let dtype = Dtype.Ir.untype' dtype in
    { id; d = { dtype; creation_code_pos; init; kind } }
end

module U = UnguardedMemRom_u

type ('kind, 'a) t = { u : U.t } [@@deriving sexp_of]
type 'a ug_mem = ([ `Mem ], 'a) t
type 'a ug_rom = ([ `Rom ], 'a) t

let create_ug_mem ?loc dtype (arr : 'a array) : 'a ug_mem =
  {
    u =
      U.create dtype (Any.Array.of_magic arr) (Code_pos.value_or_psite loc) `Mem;
  }

let create_ug_rom ?loc dtype (arr : 'a array) : 'a ug_rom =
  {
    u =
      U.create dtype (Any.Array.of_magic arr) (Code_pos.value_or_psite loc) `Rom;
  }

module Ir = struct
  include UnguardedMemRom_u

  let unwrap_ug_mem t = t.u
  let unwrap_ug_rom t = t.u
end
