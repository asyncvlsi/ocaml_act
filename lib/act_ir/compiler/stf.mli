open! Core

module Var : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Chan : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Par_split : sig
  type t = { in_v : Var.t; out_vs : Var.t option list } [@@deriving sexp_of]
end

module Par_merge : sig
  type t = { in_vs : Var.t option list; out_v : Var.t } [@@deriving sexp_of]
end

module DoWhile_phi : sig
  type t = {
    init_v : Var.t option;
    body_in_v : Var.t option;
    body_out_v : Var.t option;
    out_v : Var.t option;
  }
  [@@deriving sexp_of]
end

module Select_split : sig
  type t = { in_v : Var.t; out_vs : Var.t option list } [@@deriving sexp_of]
end

module Select_merge : sig
  type t = { in_vs : Var.t list; out_v : Var.t } [@@deriving sexp_of]
end

module Stmt : sig
  type t =
    | Nop
    | Assign of Var.t * Var.t Ir.Expr.t
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t Ir.Expr.t
    | Seq of t list
    | Par of Par_split.t list * t list * Par_merge.t list
    | SelectImm of
        Var.t Ir.Expr.t list
        * Select_split.t list
        * t list
        * Select_merge.t list
    | DoWhile of DoWhile_phi.t list * t * Var.t Ir.Expr.t
  [@@deriving sexp_of]
end

module Proc : sig
  type t = {
    stmt : Stmt.t;
    iports : (Interproc_chan.t * Chan.t) list;
    oports : (Interproc_chan.t * Chan.t) list;
  }
  [@@deriving sexp_of]
end

val stf_of_dflowable_chp_proc : Flat_chp.Proc.t -> Proc.t
val optimize_proc : Proc.t -> Proc.t
