open! Core
open! Act

type t

val create :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  t
