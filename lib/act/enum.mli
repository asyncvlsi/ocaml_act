open! Core

module type E_S = sig
  type elt
  type t = elt Expr.Wrap.t

  val var : elt Var.Wrap.t -> elt Expr.Wrap.t
  val const : elt -> elt Expr.Wrap.t
  val eq : elt Expr.Wrap.t -> elt Expr.Wrap.t -> Cbool0.t Expr.Wrap.t
  val to_int : elt Expr.Wrap.t -> Cint0.t Expr.Wrap.t
  val of_int : Cint0.t Expr.Wrap.t -> elt Expr.Wrap.t
end

module type N_S = sig
  type elt
  type t = Node.Wrap.t

  val match_ : elt Expr.Wrap.t -> f:(elt -> t) -> t
end

module type S = sig
  type t

  include Sexpable with type t := t
  include Comparable with type t := t
  include Hashable with type t := t

  val dtype : t Dtype.Wrap.t
  val bitwidth : t -> int
  val to_int : t -> Cint0.t
  val of_int : Cint0.t -> t

  module E : E_S with type elt := t
  module N : N_S with type elt := t
end

module Make (X : sig
  type t [@@deriving sexp, hash, compare, equal]

  val mapping : (t * Cint0.t) list
end) : S with type t := X.t
