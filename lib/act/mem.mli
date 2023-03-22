open! Core

(* It is not allowed to have operation on the memory from two different
   locations in the program simultaniously. *)

type ('kind, 'a) t [@@deriving sexp_of]
type 'a ug_mem = ([ `Mem ], 'a) t
type 'a ug_rom = ([ `Rom ], 'a) t

val create_ug_mem : 'a Dtype.t -> 'a array -> 'a ug_mem
val create_ug_rom : 'a Dtype.t -> 'a array -> 'a ug_rom

(**/**)

module Internal : sig
  val unwrap_ug_mem : 'a ug_mem -> Ir_mem.t
  val unwrap_ug_rom : 'a ug_rom -> Ir_mem.t
  val dtype : ('kind, 'a) t -> 'a Ir_dtype.t
end

(**/**)
