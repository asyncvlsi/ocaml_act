open! Core

type 'a t [@@deriving sexp_of]

module Internal : sig
  val unwrap : 'a t -> 'a Ir_dtype.t
  val wrap : 'a Ir_dtype.t -> 'a t
end
