open! Core

module Dflow_id : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Stmt : sig
  module GK : sig
    type t = One_hot | Idx [@@deriving sexp_of]
  end

  type t =
    | Assign of Dflow_id.t * Dflow_id.t Expr.t
    | Split of GK.t * Dflow_id.t * Dflow_id.t * Dflow_id.t option list
    | Merge of GK.t * Dflow_id.t * Dflow_id.t list * Dflow_id.t
    | Copy_init of (*dst *) Dflow_id.t * (*src*) Dflow_id.t * Act.CInt.t
  [@@deriving sexp_of]
end

module Proc : sig
  type t = {
    stmt : Stmt.t list;
    iports : (Interproc_chan.t * Dflow_id.t) list;
    oports : (Interproc_chan.t * Dflow_id.t) list;
  }
  [@@deriving sexp_of]
end

val dflow_of_stf : Stf.Proc.t -> Proc.t
val optimize_proc : Proc.t -> Proc.t
