open! Core

module Inner = struct
  module D = struct
    type t = {
      mutable wait_readable_code_pos : Act_ir.Utils.Code_pos.t option;
      mutable wait_sendable_code_pos : Act_ir.Utils.Code_pos.t option;
    }
  end

  module T = struct
    type t = {
      c : Act_ir.Ir.Chan.t;
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

  type 'a t = { u : U.t; dtype : 'a Ir_dtype.t } [@@deriving sexp_of]
end

module W = struct
  module U = Inner

  type 'a t = { u : U.t; dtype : 'a Ir_dtype.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

let create (dtype : 'a Dtype.t) : 'a t =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let dtype = Dtype.Internal.unwrap dtype in
  let bitwidth = match dtype.layout with Bits_fixed bitwidth -> bitwidth in
  let u =
    {
      Inner.c = Act_ir.Ir.Chan.create bitwidth loc;
      d = { wait_sendable_code_pos = None; wait_readable_code_pos = None };
    }
  in
  { r = { u; dtype }; w = { u; dtype } }

module Internal = struct
  let unwrap_r_inner t = t.R.u
  let unwrap_w_inner t = t.W.u
  let unwrap_r t = t.R.u.c
  let unwrap_w t = t.W.u.c
  let unwrap_ru t = t.Inner.c
  let unwrap_wu t = t.Inner.c
  let r_of_w t = { R.u = t.W.u; dtype = t.dtype }
  let w_of_r t = { W.u = t.R.u; dtype = t.dtype }
  let ru_of_wu t = t
  let wu_of_ru t = t
  let dtype_r r = r.R.dtype
  let dtype_w w = w.W.dtype
end
