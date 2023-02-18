open! Core
open! Act

type t = Chp_exporter.t
type dflow = Dflow_exporter.t

val create :
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Chp_exporter.t

val create_dflow :
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Dflow_exporter.t

val simple_ir_sim :
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Sim.t

val stf_sim :
  ?optimize:bool ->
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  Sim.t
