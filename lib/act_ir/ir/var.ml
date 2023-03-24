open! Core
open Utils

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
    init : CInt.t option;
    bitwidth : int;
  }
  [@@deriving hash, compare, equal, sexp]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create bitwidth creation_code_pos init =
  let id = Id.create () in
  { id; creation_code_pos; init; bitwidth }
