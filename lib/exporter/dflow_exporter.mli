open! Core
open! Act

module Var : sig
  type t
end

type t

val create :
  Chp.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  t

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
