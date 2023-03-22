open! Core

type t [@@deriving sexp_of]

val of_chp :
  ?with_dflow_interface:bool ->
  Chp.t ->
  iports:Chan.R.U.t list ->
  oports:Chan.W.U.t list ->
  t

val of_procs : t list -> iports:Chan.R.U.t list -> oports:Chan.W.U.t list -> t

(**/**)

module Internal : sig
  val wrap : Act_ir.Ir.Process.t -> t
  val unwrap : t -> Act_ir.Ir.Process.t
end

(**/**)
