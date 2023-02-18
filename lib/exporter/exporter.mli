open! Core

val export_program :
  Act.Program.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  unit

val export_chp :
  ?as_dflow:bool ->
  Act.Chp.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  unit
