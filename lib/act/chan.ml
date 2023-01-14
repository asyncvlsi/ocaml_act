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

module Chan_ = struct
  module U = struct
    module Id = Id

    module D = struct
      type t = {
        dtype : Any.t Dtype.Ir.t;
        creation_code_pos : Code_pos.t;
        (* I have not come up with a way to add which direction is passive into
             the type system. These two fields help with error reporting *)
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

    let create dtype creation_code_pos =
      let id = Id.create () in
      let dtype = Dtype.Ir.untype' dtype in
      let d =
        {
          D.dtype;
          creation_code_pos;
          wait_readable_code_pos = None;
          wait_sendable_code_pos = None;
        }
      in
      { id; d }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc (dtype : 'a Dtype.t) : 'a t =
    { u = U.create dtype (Code_pos.value_or_psite loc) }
end

module R = struct
  module U = Chan_.U

  type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

  let create = Chan_.create
end

module W = struct
  module U = Chan_.U

  type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

  let create = Chan_.create
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

let create ?loc (dtype : 'a Dtype.t) : 'a t =
  let c = Chan_.create ?loc dtype in
  { r = c; w = c }

module Ir = struct
  include Chan_

  let unwrap_r t = t.u
  let unwrap_w t = t.u
  let unwrap_ru t = t
  let unwrap_wu t = t
  let r_of_w t = t
  let w_of_r t = t
  let max_possible_layout_of_value t = Dtype.Ir.layout t.U.d.dtype
end
