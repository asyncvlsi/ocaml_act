open! Core

type t [@@deriving sexp_of]

val of_chp : Chp.t -> t
val dflow_iface_on_chp : Chp.t -> t

module Ir : sig
  type outer = t

  type t = Chp of Chp.Ir.t | Dflow_iface_on_chp of Chp.Ir.t
  [@@deriving sexp_of]

  val unwrap : outer -> t
end
