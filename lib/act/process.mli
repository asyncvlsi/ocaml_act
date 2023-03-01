open! Core

type t [@@deriving sexp_of]

val of_chp :
  ?with_dflow_interface:bool ->
  Chp.t ->
  iports:Chan.R.U.t list ->
  oports:Chan.W.U.t list ->
  t

val of_procs : t list -> iports:Chan.R.U.t list -> oports:Chan.W.U.t list -> t

module Ir : sig
  type outer = t

  type inner =
    | Chp of Chp.Ir.t
    | Dflow_iface_on_chp of Chp.Ir.t
    | Subprocs of t list

  and t = { inner : inner; iports : Chan.Ir.U.Set.t; oports : Chan.Ir.U.Set.t }
  [@@deriving sexp_of]

  module Inner : sig
    type nonrec t = inner =
      | Chp of Chp.Ir.t
      | Dflow_iface_on_chp of Chp.Ir.t
      | Subprocs of t list
    [@@deriving sexp_of]
  end

  val unwrap : outer -> t
end
