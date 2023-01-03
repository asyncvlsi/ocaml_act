open! Core

module Var_id : sig
  type t

  include Identifiable with type t := t

  val of_int : int -> t
end

module Expr : sig
  type t = Var of Var_id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
  [@@deriving sexp]

  val const : 'a -> t
  val map : t -> f:('a -> 'b) -> t
end

module Instr_idx : sig
  include Identifiable

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

module Send_enqueuer : sig
  type t [@@deriving sexp]

  val create : var_id:Var_id.t -> t
end

module Read_dequeuer : sig
  type t [@@deriving sexp]

  val create : var_id:Var_id.t -> equals:(Any.t -> Any.t -> bool) -> t
end

module N : sig
  type t =
    | End
    | Unreachable
    | Nop
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
    (* A few magic isntructions to allow user controled chanel operations. These instruction should be placed immediatly
       after the assoiated send/read instruction *)
    | Send_enqueuer of Send_enqueuer.t
    | Read_dequeuer of Read_dequeuer.t
  [@@deriving sexp]

  val var_ids : t -> Var_id.t list
end

module Assem_vec : sig
  type t

  val create : unit -> t
  val push : t -> N.t -> Instr_idx.t
  val edit : t -> Instr_idx.t -> N.t -> unit
  val next_idx : t -> Instr_idx.t
  val to_array : t -> N.t array
end

module Var_table : sig
  type t [@@deriving sexp]
end

module Sim : sig
  type t = {
    assem : N.t array;
    mutable pcs : Instr_idx.t Vec.t;
    mutable var_table : Var_table.t;
    rng : (Random.State.t[@sexp.opaque]);
    mutable is_done : bool;
    chan_buffs : Chan_buff.t list;
  }
  [@@deriving sexp]

  module Wait_outcome : sig
    type t =
      | Already_errored
      | Time_out
      | Stuck of
          (* how many steps the simulation ran for before getting stuck *) int
      | Assert_failure of Instr_idx.t
      | Uninit_id of Var_id.t * Instr_idx.t
      | Simul_chan_senders of Instr_idx.t * Instr_idx.t
      | Simul_chan_readers of Instr_idx.t * Instr_idx.t
      | Simul_read_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Simul_write_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Select_no_guards_true of Instr_idx.t
      | Select_multiple_guards_true of int list * Instr_idx.t
      | Read_dequeuer_wrong_value of Instr_idx.t * Any.t * Any.t
      | Read_dequeuer_not_done of Instr_idx.t
      | Send_enqueuer_not_done of Instr_idx.t
    [@@deriving sexp]
  end

  val create : ?seed:int -> N.t array -> var_ct:int -> t
  val wait : ?max_steps:int -> t -> Wait_outcome.t

  module Advanced : sig
    val set_user_sends :
      t -> values:Any.t Queue.t -> send_instr:Instr_idx.t -> unit

    val set_user_reads :
      t -> values:Any.t Queue.t -> read_instr:Instr_idx.t -> unit
  end
end
