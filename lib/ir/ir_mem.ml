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

module D = struct
  type t = {
    dtype : Any.t Ir_dtype.t;
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

let create dtype creation_code_pos init kind =
  let dtype = Ir_dtype.untype dtype in
  let init = Any.Array.of_magic init in
  let id = Id.create () in
  { id; d = { dtype; creation_code_pos; init; kind } }
