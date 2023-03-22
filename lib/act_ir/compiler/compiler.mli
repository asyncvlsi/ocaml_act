open! Core

module Compiled_program : sig
  type t [@@deriving sexp_of]
end

val compile :
  Ir.Process.t -> to_:[ `Prod_rules | `Chp_and_dataflow ] -> Compiled_program.t

val export : Compiled_program.t -> string
val export_print : Compiled_program.t -> unit
val sim : ?seed:int -> Compiled_program.t -> Sim.t
