open! Core

(**/**)

module Inner : sig
  module D : sig
    type t = {
      mutable wait_readable_code_pos : Code_pos.t option;
      mutable wait_sendable_code_pos : Code_pos.t option;
      dtype : Any.t Ir_dtype.t;
    }
  end

  type t = {
    c : Ir_chan.t;
    d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
  }
  [@@deriving sexp_of]
end

(**/**)

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
  val dtype_r : 'a R.t -> 'a Ir_dtype.t
  val dtype_w : 'a W.t -> 'a Ir_dtype.t
  val wrap_any : Ir_chan.t -> Any.t t
  val wrap_'a : Ir_chan.t -> 'a t
  val unwrap_r_inner : 'a R.t -> Inner.t
  val unwrap_w_inner : 'a W.t -> Inner.t
  val unwrap_r : 'a R.t -> Ir_chan.t
  val unwrap_w : 'a W.t -> Ir_chan.t
  val unwrap_ru : R.U.t -> Ir_chan.t
  val unwrap_wu : W.U.t -> Ir_chan.t
  val wrap_ru : Ir_chan.t -> R.U.t
  val wrap_wu : Ir_chan.t -> W.U.t
  val wrap_r : Ir_chan.t -> Any.t R.t
  val wrap_w : Ir_chan.t -> Any.t W.t
  val r_of_w : 'a W.t -> 'a R.t
  val w_of_r : 'a R.t -> 'a W.t
  val ru_of_wu : W.U.t -> R.U.t
  val wu_of_ru : R.U.t -> W.U.t
end

(**/**)
