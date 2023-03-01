open! Core

module Var : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Stmt : sig
  module Guard : sig
    type t = One_hot of Var.t | Idx of Var.t | Bits of Var.t list
    [@@deriving sexp_of]

    val ids : t -> Var.t list
  end

  type t =
    | MultiAssign of (Var.t * Var.t Expr.t) list
    | Split of Guard.t * Var.t * Var.t option list
    | Merge of Guard.t * Var.t list * Var.t
    | Copy_init of (*dst *) Var.t * (*src*) Var.t * Act.CInt.t
  [@@deriving sexp_of]
end

module Proc : sig
  type t = {
    stmt : Stmt.t list;
    iports : (Interproc_chan.t * Var.t) list;
    oports : (Interproc_chan.t * Var.t) list;
  }
  [@@deriving sexp_of]
end

val var_ids :
  Stmt.t list ->
  (Interproc_chan.t * Var.t) list ->
  (Interproc_chan.t * Var.t) list ->
  Var.Set.t

val dflow_of_stf : Stf.Proc.t -> Proc.t
val optimize_proc : Proc.t -> Proc.t
