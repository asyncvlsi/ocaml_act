open! Core

module type E_S = sig
  type elt
  type t = elt Expr.t

  val var : elt Var.t -> elt Expr.t
  val const : elt -> elt Expr.t
  val eq : elt Expr.t -> elt Expr.t -> Cbool0.t Expr.t
  val to_int : elt Expr.t -> Act_ir.CInt.t Expr.t
  val of_int : Act_ir.CInt.t Expr.t -> elt Expr.t
end

module type Chp_S = sig
  type elt
  type t = Chp.t

  val match_ : elt Expr.t -> f:(elt -> t) -> t
end

module type S = sig
  type t

  include Sexpable with type t := t
  include Comparable with type t := t
  include Hashable with type t := t

  val dtype : t Dtype.t
  val bitwidth : t -> int
  val to_int : t -> Act_ir.CInt.t
  val of_int : Act_ir.CInt.t -> t option

  module E : E_S with type elt := t
  module Chp : Chp_S with type elt := t
end

module Make (X : sig
  type t [@@deriving sexp, hash, compare, equal]

  val mapping : (t * Act_ir.CInt.t) list
end) : S with type t := X.t
