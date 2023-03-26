open! Core
open Utils

module Tag : sig
  type t = int

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Expr = Ir.Expr

module Var : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Chan : sig
  type t = { id : int; bitwidth : int } [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Mem : sig
  type t = { id : int; cell_bitwidth : int; init : CInt.t array }
  [@@deriving sexp_of]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Probe : sig
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

module E : sig
  type t =
    | Uninit_id of Var.t * Tag.t
    | Simul_read_write_var of Tag.t * Tag.t * Var.t
    | Simul_write_write_var of Tag.t * Tag.t * Var.t
    | Simul_mem_access of Tag.t * Tag.t * Mem.t
    | Select_no_guards_true of Tag.t
    | Select_multiple_guards_true of Tag.t * int list
    | Assert_failure of Tag.t * CInt.t
    | Simul_chan_readers of Tag.t * Tag.t
    | Simul_chan_senders of Tag.t * Tag.t
    | Select_multiple_true_probes of Tag.t * (int * (Probe.t * int)) list
    | Unstable_probe of Tag.t * Probe.t
    | Read_dequeuer_wrong_value of Chan.t * CInt.t * int
    | Mem_out_of_bounds of Tag.t * CInt.t * int
    | User_read_did_not_complete of Chan.t * int
    | User_send_did_not_complete of Chan.t * int
    | Stuck
    | Time_out
  [@@deriving sexp_of]
end

module Stmt : sig
  type t =
    | Nop of Tag.t
    | Assign of Tag.t * Var.t * Var.t Expr.t
    | Log1 of Tag.t * Var.t Expr.t
    | Assert of Tag.t * Var.t Expr.t * Var.t Expr.t
    | Seq of Tag.t * t list
    | Par of Tag.t * t list
    | SelectImm of Tag.t * (Var.t Expr.t * t) list * t
    | Read of Tag.t * Chan.t * Var.t
    | Send of Tag.t * Chan.t * Var.t Expr.t
    | WhileLoop of Tag.t * Var.t Expr.t * t
    | DoWhile of Tag.t * t * Var.t Expr.t
    | ReadMem of Tag.t * Mem.t * Var.t Expr.t * Var.t
    | WriteMem of Tag.t * Mem.t * Var.t Expr.t * Var.t Expr.t
    | Nondeterm_select of Tag.t * (Probe.t * t) list
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : seed:int -> Stmt.t -> iports:Chan.t list -> oports:Chan.t list -> t

val wait :
  t ->
  max_steps:int ->
  to_send:(Chan.t * CInt.t array) list ->
  to_read:(Chan.t * CInt.t array) list ->
  (Tag.t * CInt.t) list * E.t

val reset : t -> unit
