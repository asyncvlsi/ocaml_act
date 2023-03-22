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
  type t = { id : Id.t; bitwidth : int; creation_code_pos : Code_pos.t }
  [@@deriving hash, compare, equal, sexp]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create dtype creation_code_pos =
  let id = Id.create () in
  let bitwidth =
    match dtype.Ir_dtype.layout with Bits_fixed bitwidth -> bitwidth
  in
  { id; bitwidth; creation_code_pos }
