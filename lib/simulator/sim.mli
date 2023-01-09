open! Core
open! Act

type t

val create :
  N.t ->
  user_sendable_ports:Chan.W.U.t list ->
  user_readable_ports:Chan.R.U.t list ->
  t

val wait : t -> ?max_steps:int -> ?line_numbers:bool -> unit -> unit Or_error.t
val wait' : t -> ?max_steps:int -> unit -> unit
val send : t -> ?loc:Code_pos.t -> 'a Chan.W.t -> 'a -> unit
val read : t -> ?loc:Code_pos.t -> 'a Chan.R.t -> 'a -> unit
