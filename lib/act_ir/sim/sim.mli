open! Core

type t [@@deriving sexp_of]

val simulate : ?seed:int -> Ir.Process.t -> t
val wait : t -> ?max_steps:int -> ?line_numbers:bool -> unit -> unit Or_error.t
val wait' : t -> ?max_steps:int -> unit -> unit
val send : t -> Ir.Chan.t -> Cint.t -> unit
val read : t -> Ir.Chan.t -> Cint.t -> unit
