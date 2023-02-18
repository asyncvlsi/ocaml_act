open! Core

module Dflow_id : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Stmt : sig
  type t =
    | Assign of Dflow_id.t * Dflow_id.t Expr.t
    | Rename_assign of Dflow_id.t * Dflow_id.t
    | Split of Dflow_id.t * Dflow_id.t * Dflow_id.t option list
    | Merge of Dflow_id.t * Dflow_id.t list * Dflow_id.t
    | MergeBoolGuard of Dflow_id.t * (Dflow_id.t * Dflow_id.t) * Dflow_id.t
    | SplitBoolGuard of
        Dflow_id.t * Dflow_id.t * (Dflow_id.t option * Dflow_id.t option)
    | Copy_init of Dflow_id.t * Dflow_id.t * Act.CInt.t
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
val optimize_dflow : Proc.t -> Proc.t
