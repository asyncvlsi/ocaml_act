open! Core

type t [@@deriving sexp_of]

val of_procs : Process.t list -> t

module Ir : sig
  type outer = t
  type t = Process.Ir.t list [@@deriving sexp_of]

  val unwrap : outer -> t
end
