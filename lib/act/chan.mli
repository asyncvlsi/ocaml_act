open! Core

module R : sig
  module U : sig
    type t [@@deriving sexp, hash, equal, compare]

    include Hashable with type t := t
    include Comparable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a Dtype.t -> 'a t
end

module W : sig
  module U : sig
    type t [@@deriving sexp, hash, equal, compare]

    include Hashable with type t := t
    include Comparable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a Dtype.t -> 'a t
end

type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

val create : ?loc:Code_pos.t -> 'a Dtype.t -> 'a t

module Ir : sig
  module U : sig
    module Id : Identifiable

    module D : sig
      type t = {
        dtype : Any.t Dtype.Ir.t;
        creation_code_pos : Code_pos.t;
        (* I have not come up with a way to add which direction is passive into
           the type system. These two fields help with error reporting *)
        mutable wait_readable_code_pos : Code_pos.t option;
        mutable wait_sendable_code_pos : Code_pos.t option;
      }
    end

    type t = {
      id : Id.t;
      d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
    }
    [@@deriving sexp_of]

    include Comparable with type t := t
    include Hashable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val unwrap_r : 'a R.t -> U.t
  val unwrap_w : 'a W.t -> U.t
  val wrap_ru : U.t -> R.U.t
  val wrap_wu : U.t -> W.U.t
  val unwrap_ru : R.U.t -> U.t
  val unwrap_wu : W.U.t -> U.t
  val r_of_w : 'a W.t -> 'a R.t
  val w_of_r : 'a R.t -> 'a W.t
  val ru_of_wu : W.U.t -> R.U.t
  val wu_of_ru : R.U.t -> W.U.t
  val max_possible_layout_of_value : U.t -> Layout.t
end
