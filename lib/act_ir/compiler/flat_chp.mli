open! Core

module Var : sig
  module Id = Int

  type t = { id : Id.t; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable with type t := t
  include Hashable with type t := t

  val bitwidth : t -> int
end

module Chan : sig
  module Id = Int

  type t = { id : Id.t; bitwidth : int } [@@deriving sexp, hash, equal, compare]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Probe : sig
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

module Stmt : sig
  type t =
    | Nop
    | Assert of Var.t Ir.Expr.t
    | Assign of Var.t * Var.t Ir.Expr.t
    | Seq of t list
    | Par of t list
    (* assert happens immediatly after read before any other code runs *)
    | ReadThenAssert of Chan.t * Var.t * Var.t Ir.Expr.t
    | Send of Chan.t * Var.t Ir.Expr.t
    | DoWhile of t * Var.t Ir.Expr.t
      (* This expr is a one-hot vector with List.length branches bits indexing
         into the list of branches *)
    | SelectImm of Var.t Ir.Expr.t list * t list
    | Nondeterm_select of (Probe.t * t) list
  [@@deriving sexp_of]

  val flatten : t -> t
end

module Proc : sig
  type t = {
    dflowable : bool;
    stmt : Stmt.t;
    iports : (Interproc_chan.t * Chan.t) list;
    oports : (Interproc_chan.t * Chan.t) list;
  }
  [@@deriving sexp_of]
end

val of_chp :
  Ir.Chp.t ->
  new_interproc_chan:(int -> Interproc_chan.t) ->
  interproc_chan_of_ir_chan:(Ir.Chan.t -> Interproc_chan.t) ->
  dflowable:bool ->
  Proc.t
  * ((Interproc_chan.t * Interproc_chan.t option * Interproc_chan.t option)
    * (int * int))
    Ir.Mem.Map.t
