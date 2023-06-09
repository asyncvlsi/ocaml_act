open! Core

(**/**)

module Inner : sig
  module D : sig
    type t = {
      mutable wait_readable_code_pos : Act_ir.Utils.Code_pos.t option;
      mutable wait_sendable_code_pos : Act_ir.Utils.Code_pos.t option;
    }
  end

  type t = {
    c : Act_ir.Ir.Chan.t;
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

  type 'a t = { u : U.t; dtype : 'a Ir_dtype.t } [@@deriving sexp_of]
end

module W : sig
  module U : sig
    type t [@@deriving sexp_of, hash, equal, compare]

    include Hashable with type t := t
    include Comparable with type t := t
  end

  type 'a t = { u : U.t; dtype : 'a Ir_dtype.t } [@@deriving sexp_of]
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

val create : 'a Dtype.t -> 'a t

(**/**)

module Internal : sig
  val dtype_r : 'a R.t -> 'a Ir_dtype.t
  val dtype_w : 'a W.t -> 'a Ir_dtype.t
  val unwrap_r_inner : 'a R.t -> Inner.t
  val unwrap_w_inner : 'a W.t -> Inner.t
  val unwrap_r : 'a R.t -> Act_ir.Ir.Chan.t
  val unwrap_w : 'a W.t -> Act_ir.Ir.Chan.t
  val unwrap_ru : R.U.t -> Act_ir.Ir.Chan.t
  val unwrap_wu : W.U.t -> Act_ir.Ir.Chan.t
  val r_of_w : 'a W.t -> 'a R.t
  val w_of_r : 'a R.t -> 'a W.t
  val ru_of_wu : W.U.t -> R.U.t
  val wu_of_ru : R.U.t -> W.U.t
end

(**/**)
