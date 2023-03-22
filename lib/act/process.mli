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
  val wrap : Ir_process.t -> t
  val unwrap : t -> Ir_process.t
end

(**/**)
