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
    (* I have not come up with a way to add which direction is passive into the
       type system. These two fields help with error reporting *)
    mutable wait_readable_code_pos : Code_pos.t option;
    mutable wait_sendable_code_pos : Code_pos.t option;
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

let max_possible_layout_of_value t = Ir_dtype.layout t.d.dtype

let create dtype creation_code_pos =
  let id = Id.create () in
  let dtype = Ir_dtype.untype dtype in
  let d =
    {
      D.dtype;
      creation_code_pos;
      wait_readable_code_pos = None;
      wait_sendable_code_pos = None;
    }
  in
   { id; d }
