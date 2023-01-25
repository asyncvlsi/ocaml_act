open! Core

module Wrap : sig
  module U : Comparable_and_hashable.S

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> ?init:'a -> 'a Dtype.Wrap.t -> 'a t
end

(* The internal data structures. These are only meant to be constructed throguh the above interfaces. *)
module Ir : sig
  module U : sig
    module Id : Identifiable

    module D : sig
      type t = {
        dtype : Any.t Dtype.Ir.t;
        creation_code_pos : Code_pos.t;
        init : Any.t option;
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

  val unwrap : 'a Wrap.t -> 'a t
  val untype : 'a t -> U.t
  val untype' : 'a Wrap.t -> U.t
end
