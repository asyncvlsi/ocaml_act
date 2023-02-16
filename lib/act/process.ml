open! Core

module Ir = struct
  type t = Chp of Chp.Ir.t | Dflow_iface_on_chp of Chp.Ir.t
  [@@deriving sexp_of]

  type outer = t

  let unwrap t = t
  let of_chp t = (* TODO do checks *) Chp (Chp.Ir.unwrap t)

  let dflow_iface_on_chp t =
    (* TODO do checks *) Dflow_iface_on_chp (Chp.Ir.unwrap t)
end

include Ir
