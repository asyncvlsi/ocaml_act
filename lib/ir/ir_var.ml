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
    creation_code_pos : Code_pos.t;
    init : Cint0.t option;
    bitwidth : int;
  }
  [@@deriving hash, compare, equal, sexp]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create dtype init creation_code_pos =
  let id = Id.create () in
  let init = Option.map init ~f:dtype.Ir_dtype.cint_of in
  let bitwidth = match dtype.layout with Bits_fixed bitwidth -> bitwidth in
  { id; creation_code_pos; init; bitwidth }

let create dtype creation_code_pos init = create dtype init creation_code_pos
