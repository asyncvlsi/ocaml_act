open! Core
open! Act

type t = Chp_exporter.t
type dflow = Dflow_exporter.t

val create :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Chp_exporter.t

val create_dflow :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Dflow_exporter.t

val simple_ir_sim :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Sim.t

val stf_sim :
  ?optimize:bool ->
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Sim.t
