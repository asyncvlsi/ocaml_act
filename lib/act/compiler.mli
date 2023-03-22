open! Core

module Compiled_program : sig
  type t [@@deriving sexp_of]
end

val compile :
  Process.t -> to_:[ `Prod_rules | `Chp_and_dataflow ] -> Compiled_program.t

val compile_chp :
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  to_:[ `Prod_rules | `Chp | `Dataflow ] ->
  Compiled_program.t

val export : Compiled_program.t -> string
val export_print : Compiled_program.t -> unit
val sim : ?seed:int -> Compiled_program.t -> Sim.t
