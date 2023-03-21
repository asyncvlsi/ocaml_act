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

module U = struct
  module Id = Id

  module D = struct
    type t = {
      dtype : Any.t Ir_dtype.t;
      creation_code_pos : Code_pos.t;
      init : Any.t option;
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

  let create dtype init creation_code_pos =
    let id = Id.create () in
    let dtype = Ir_dtype.untype dtype in
    let init = Any.Option.of_magic init in
    { id; d = { dtype; creation_code_pos; init } }
end

type 'a t = { u : U.t } [@@deriving sexp_of]

let create dtype creation_code_pos init =
  { u = U.create dtype init creation_code_pos }

let untype t = Obj.magic t
