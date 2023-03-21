open! Core

module R : sig
  module U : sig
    type t [@@deriving sexp_of, hash, equal, compare]

    include Hashable with type t := t
    include Comparable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

module W : sig
  module U : sig
    type t [@@deriving sexp_of, hash, equal, compare]

    include Hashable with type t := t
    include Comparable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

val create : 'a Dtype.t -> 'a t

(**/**)

module Internal : sig
  val wrap_any : Ir_chan.U.t -> Any.t t
  val wrap_'a : Ir_chan.U.t -> 'a t
  val unwrap_r : 'a R.t -> Ir_chan.U.t
  val unwrap_w : 'a W.t -> Ir_chan.U.t
  val unwrap_ru : R.U.t -> Ir_chan.U.t
  val unwrap_wu : W.U.t -> Ir_chan.U.t
  val wrap_ru : Ir_chan.U.t -> R.U.t
  val wrap_wu : Ir_chan.U.t -> W.U.t
  val wrap_r : Ir_chan.U.t -> Any.t R.t
  val wrap_w : Ir_chan.U.t -> Any.t W.t
  val r_of_w : 'a W.t -> 'a R.t
  val w_of_r : 'a R.t -> 'a W.t
  val ru_of_wu : W.U.t -> R.U.t
  val wu_of_ru : R.U.t -> W.U.t
end

(**/**)
