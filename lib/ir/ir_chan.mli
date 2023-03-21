open! Core

module U : sig
  module Id : Identifiable

  module D : sig
    type t = {
      dtype : Any.t Ir_dtype.t;
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

val max_possible_layout_of_value : U.t -> Ir_layout.t
val create : 'a Ir_dtype.t -> Code_pos.t -> 'a t
