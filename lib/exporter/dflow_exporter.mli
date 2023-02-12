open! Core
open! Act

module Var : sig
  type t
end

type t

val create :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  t

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
