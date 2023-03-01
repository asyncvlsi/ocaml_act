open! Core

val compile_chp :
  Act.Chp.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  to_:[ `Prod_rules | `Chp | `Dataflow ] ->
  string

val compile : Act.Process.t -> to_:[ `Prod_rules | `Chp_and_dataflow ] -> string
