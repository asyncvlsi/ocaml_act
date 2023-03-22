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

module T = struct
  type t = {
    id : Id.t;
    cell_bitwidth : int;
    creation_code_pos : Code_pos.t;
    init : (Cint0.t array[@hash.ignore] [@compare.ignore] [@equal.ignore]);
    kind : [ `Mem | `Rom ];
  }
  [@@deriving hash, compare, equal, sexp]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create cell_bitwidth creation_code_pos init kind =
  let id = Id.create () in
  { id; cell_bitwidth; creation_code_pos; init; kind }
