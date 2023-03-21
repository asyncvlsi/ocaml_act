open! Core

module U : sig
  module Id : Identifiable

  module D : sig
    type t = {
      dtype : Any.t Ir_dtype.t;
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

val create : 'a Ir_dtype.t -> Code_pos.t -> 'a option -> 'a t
val untype : 'a t -> Any.t t
