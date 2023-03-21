open! Core

module R = struct
  module U = Ir_chan.U

  type 'a t = 'a Ir_chan.t = { u : U.t } [@@deriving sexp_of]
end

module W = struct
  module U = Ir_chan.U

  type 'a t = 'a Ir_chan.t = { u : U.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

let create (dtype : 'a Dtype.t) : 'a t =
  let loc = Code_pos.psite () in
  let dtype = Dtype.Internal.unwrap dtype in
  let c = Ir_chan.create dtype loc in
  { r = c; w = c }

module Internal = struct
  let wrap_'a (t : Ir_chan.U.t) : 'a t = { r = { u = t }; w = { u = t } }
  let wrap_any (t : Ir_chan.U.t) : Any.t t = wrap_'a t
  let unwrap_r t = t.Ir_chan.u
  let unwrap_w t = t.Ir_chan.u
  let unwrap_ru t = t
  let unwrap_wu t = t
  let wrap_ru t = t
  let wrap_wu t = t
  let wrap_r t = { R.u = t }
  let wrap_w t = { W.u = t }
  let r_of_w t = t
  let w_of_r t = t
  let ru_of_wu t = t
  let wu_of_ru t = t
end
