open! Core

module Var_id : sig
  type t

  include Identifiable with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Expr : sig
  type t = Var of Var_id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
  [@@deriving sexp]

  val const : 'a -> t
  val map : t -> f:('a -> 'b) -> t
end

module Instr_idx : sig
  type t

  val dummy_val : t
  val next : t -> t
end

module Chan_buff : sig
  type t

  val create : unit -> t
end

module Par_join : sig
  type t

  val create : max_ct:int -> t
end

module N : sig
  type t =
    | End
    | Unreachable
    | ConsumePC
    | Assign of Var_id.t * Expr.t
    | Log of Expr.t
    | Assert of Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
    | SelectImm of (Expr.t * Instr_idx.t) list
    | SelectImmElse of (Expr.t * Instr_idx.t) list * Instr_idx.t
    | Read of Var_id.t * Chan_buff.t
    | Send of Expr.t * Chan_buff.t
  [@@deriving sexp]

  val var_ids : t -> Var_id.t list
end

module Builder : sig
  type t

  val create : unit -> t
  val push : t -> N.t -> Instr_idx.t
  val edit : t -> Instr_idx.t -> N.t -> unit
  val next_idx : t -> Instr_idx.t
  val to_array : t -> N.t array
end

module Sim : sig
  type t

  module Wait_outcome : sig
    type t =
      | Done
      | Already_done
      | Time_out
      | Stuck
      | Assert_failure of Instr_idx.t
      | Uninit_id of Var_id.t * Instr_idx.t
      | Simul_chan_senders of Instr_idx.t * Instr_idx.t
      | Simul_chan_readers of Instr_idx.t * Instr_idx.t
      | Simul_read_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Simul_write_write of Var_id.t * Instr_idx.t * Instr_idx.t
    [@@deriving sexp]
  end

  val create : ?seed:int -> N.t array -> var_ct:int -> t
  val wait : ?max_steps:int -> t -> Wait_outcome.t

  module Advanced : sig
    val add_pc : t -> Instr_idx.t -> unit
    val set_var : t -> var_id:Var_id.t -> value:Any.t -> unit
  end
end
