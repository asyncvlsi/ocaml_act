open! Core

type 'a t [@@deriving sexp_of]

val create : ?init:'a -> 'a Dtype.t -> 'a t

(**/**)

module Internal : sig
  val unwrap : 'a t -> Act_ir.Ir.Var.t
  val dtype : 'a t -> 'a Ir_dtype.t
end

(**/**)
