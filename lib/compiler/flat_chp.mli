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
    | Assert of Var.t F_expr.t
    | Assign of Var.t * Var.t F_expr.t
    | Seq of t list
    | Par of t list
    (* assert happens immediatly after read before any other code runs *)
    | ReadThenAssert of Chan.t * Var.t * Var.t F_expr.t
    | Send of Chan.t * Var.t F_expr.t
    | DoWhile of t * Var.t F_expr.t
      (* This expr is a one-hot vector with List.length branches bits indexing
         into the list of branches *)
    | SelectImm of Var.t F_expr.t list * t list
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
  Ir_chp.t ->
  new_interproc_chan:(int -> Interproc_chan.t) ->
  interproc_chan_of_ir_chan:(Ir_chan.U.t -> Interproc_chan.t) ->
  dflowable:bool ->
  Proc.t
  * ((Interproc_chan.t * Interproc_chan.t option * Interproc_chan.t option)
    * (int * int))
    Ir_mem.Map.t
