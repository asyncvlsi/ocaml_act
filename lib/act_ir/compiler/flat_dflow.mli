open! Core
open Utils

module Var : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module FBlock : sig
  include Fblock.S with type var := Var.t
end

module Stmt : sig
  type t =
    | MultiAssign of FBlock.t
    | Split of Var.t * Var.t * Var.t option list
    | Merge of Var.t * Var.t list * Var.t
    | Buff1 of (*dst *) Var.t * (*src*) Var.t * CInt.t option
    | Clone of Var.t * Var.t list
    | Sink of Var.t
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

val of_chp : Flat_chp.Proc.t -> Proc.t
val var_ids : Proc.t -> Var.Set.t
