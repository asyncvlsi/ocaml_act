open! Core

type t

val create :
  Ir.N.t ->
  user_sendable_ports:Ir.Chan.W.U.t list ->
  user_readable_ports:Ir.Chan.R.U.t list ->
  t

val wait : t -> ?max_steps:int -> ?line_numbers:bool -> unit -> unit Or_error.t
val wait' : t -> ?max_steps:int -> unit -> unit
val send : t -> ?loc:Code_pos.t -> 'a Ir.Chan.W.t -> 'a -> unit
val read : t -> ?loc:Code_pos.t -> 'a Ir.Chan.R.t -> 'a -> unit
