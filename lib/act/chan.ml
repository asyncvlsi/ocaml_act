open! Core

module R = struct
  module U = Ir_chan

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

module W = struct
  module U = Ir_chan

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

let create (dtype : 'a Dtype.t) : 'a t =
  let loc = Code_pos.psite () in
  let dtype = Dtype.Internal.unwrap dtype in
  let c = Ir_chan.create dtype loc in
  { r = {u = c}; w = { u = c} }

module Internal = struct
  let wrap_'a (t : Ir_chan.t) : 'a t = { r = { u = t }; w = { u = t } }
  let wrap_any (t : Ir_chan.t) : Any.t t = wrap_'a t
  let unwrap_r t = t.R.u
  let unwrap_w t = t.W.u
  let unwrap_ru t = t
  let unwrap_wu t = t
  let wrap_ru t = t
  let wrap_wu t = t
  let wrap_r t = { R.u = t }
  let wrap_w t = { W.u = t }
  let r_of_w t = {R.u = t.W.u}
  let w_of_r t = {W.u = t.R.u}
  let ru_of_wu t = t
  let wu_of_ru t = t
end
