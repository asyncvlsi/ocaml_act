open! Core

type ('v, 'vc) t
type ('v, 'vc) outer = ('v, 'vc) t

module type S = sig
  type var
  type comparator_witness
  type t = (var, comparator_witness) outer [@@deriving sexp_of]

  val create1 : var -> var F_expr.t -> t
  val expr_list : t -> (var * var F_expr.t) list
  val ins : t -> var list
  val outs : t -> var list
  val deps_of_outs : t -> (var * var list) list
  val filter_outs : t -> f:(var -> bool) -> t
  val map_ins : t -> f:(var -> var) -> t
  val dup_ids : t -> var list list
  val merge_same_reads : t -> t -> t
  val append : t -> t -> t
  val get_consts_if_const : t -> (var * Cint.t) list option

  val map_ins_and_outs :
    ('v, 'vc) outer -> map_in:('v -> var) -> map_out:('v -> var) -> t
end

module Make (Var : sig
  type t [@@deriving sexp, hash, compare, equal]
  type comparator_witness

  val comparator : (t, comparator_witness) Base.Comparator.comparator
end) :
  S with type var = Var.t and type comparator_witness = Var.comparator_witness
