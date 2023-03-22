open! Core

module Inner = struct
  module D = struct
    type t = {
      mutable wait_readable_code_pos : Code_pos.t option;
      mutable wait_sendable_code_pos : Code_pos.t option;
      dtype : Any.t Ir_dtype.t;
    }
  end

  module T = struct
    type t = {
      c : Ir_chan.t;
      d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
    }
    [@@deriving hash, compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module R = struct
  module U = Inner

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

module W = struct
  module U = Inner

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

let create (dtype : 'a Dtype.t) : 'a t =
  let loc = Code_pos.psite () in
  let dtype = Dtype.Internal.unwrap dtype |> Ir_dtype.untype in
  let bitwidth = match dtype.layout with Bits_fixed bitwidth -> bitwidth in
  let u =
    {
      Inner.c = Ir_chan.create bitwidth loc;
      d =
        { dtype; wait_sendable_code_pos = None; wait_readable_code_pos = None };
    }
  in
  { r = { u }; w = { u } }

module Internal = struct
  let unwrap_r_inner t = t.R.u
  let unwrap_w_inner t = t.W.u
  let unwrap_r t = t.R.u.c
  let unwrap_w t = t.W.u.c
  let unwrap_ru t = t.Inner.c
  let unwrap_wu t = t.Inner.c
  let r_of_w t = { R.u = t.W.u }
  let w_of_r t = { W.u = t.R.u }
  let ru_of_wu t = t
  let wu_of_ru t = t
  let dtype_r r = r.R.u.d.dtype |> Obj.magic
  let dtype_w w = w.W.u.d.dtype |> Obj.magic
end
