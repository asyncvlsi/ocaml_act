open! Core

module Wrap : sig
  module R : sig
    module U : Comparable_and_hashable.S

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a Dtype.Wrap.t -> 'a t
  end

  module W : sig
    module U : Comparable_and_hashable.S

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a Dtype.Wrap.t -> 'a t
  end

  type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a Dtype.Wrap.t -> 'a t
end

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

    include Comparable with type t := t
    include Hashable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val unwrap_r : 'a Wrap.R.t -> U.t
  val unwrap_w : 'a Wrap.W.t -> U.t
  val unwrap_ru : Wrap.R.U.t -> U.t
  val unwrap_wu : Wrap.W.U.t -> U.t
  val r_of_w : 'a Wrap.W.t -> 'a Wrap.R.t
  val w_of_r : 'a Wrap.R.t -> 'a Wrap.W.t
  val max_possible_layout_of_value : U.t -> Layout.t
end
