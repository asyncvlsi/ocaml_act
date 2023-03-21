open! Core

type 'a t [@@deriving sexp_of]

val create : ?init:'a -> 'a Dtype.t -> 'a t

module Internal : sig
  val unwrap : 'a t -> 'a Ir_var.t
end
