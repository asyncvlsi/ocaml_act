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
  module U = struct
    module Id = Id

    module D = struct
      type t = {
        dtype : Any.t Dtype.Ir.t;
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
      let dtype = Dtype.Ir.untype' dtype in
      let init = Any.Option.of_magic init in
      { id; d = { dtype; creation_code_pos; init } }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc ?init (dtype : 'a Dtype.t) : 'a t =
    { u = U.create dtype init (Code_pos.value_or_psite loc) }
end

include T

module Ir = struct
  include T

  type 'a outer = 'a t

  let unwrap (t : 'a outer) = t
  let untype (t : 'a t) : U.t = t.u
  let untype' = untype
end
