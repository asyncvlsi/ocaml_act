open! Core
open Utils

(* This module doesnt know about dtypes. Everything is just a Cint. Moreover, it
   includes no recursive data types. So, this should be fairly easy to port into
   c/c++/rust if we need the extra performance *)

module Instr_idx : sig
  type t = int [@@deriving sexp, equal]

  val dummy_val : t
  val next : t -> t
end

module Var_id = Int
module Chan_id = Int
module Mem_id = Int
module Enqueuer_idx = Int
module Dequeuer_idx = Int

module Probe : sig
  type t = Read_ready of Chan_id.t | Send_ready of Chan_id.t
  [@@deriving sexp_of, equal]
end

module Expr : sig
  module NI = Int

  module N : sig
    type t =
      | Var of Var_id.t
      | Const of CInt.t
      | Add of NI.t * NI.t
      | Sub_no_underflow of NI.t * NI.t
      | Mul of NI.t * NI.t
      | Div of NI.t * NI.t
      | Mod of NI.t * NI.t
      | LShift of NI.t * NI.t
      | RShift of NI.t * NI.t
      | BitAnd of NI.t * NI.t
      | BitOr of NI.t * NI.t
      | BitXor of NI.t * NI.t
      | Eq of NI.t * NI.t
      | Ne of NI.t * NI.t
      | Lt of NI.t * NI.t
      | Le of NI.t * NI.t
      | Gt of NI.t * NI.t
      | Ge of NI.t * NI.t
      | Clip of NI.t * int
      | Return of NI.t
    [@@deriving sexp, hash, equal, compare]

    include Hashable with type t := t
  end

  type t = { ns : N.t array } [@@deriving sexp_of]

  val var_ids : t -> Var_id.t list
end

module Par_join : sig
  type t [@@deriving sexp]

  val create : max_ct:int -> t
end

module N : sig
  type t =
    | End
    | Nop
    | Assign of Var_id.t * Expr.t
    | Log1 of Expr.t
    | Assert of Expr.t * Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
    | SelectImm of (Expr.t * Instr_idx.t) list
    | SelectImmElse of (Expr.t * Instr_idx.t) list * Instr_idx.t
    | Read of Var_id.t * Chan_id.t
    | Send of Expr.t * Chan_id.t
    | ReadMem of
        (* idx *) Expr.t * (* dst *) Var_id.t * (* reg *) Var_id.t * Mem_id.t
    | WriteMem of
        (* idx *) Expr.t * (* src *) Expr.t * (* idx_reg *) Var_id.t * Mem_id.t
    (* TODO handle nondeterministic select *)
    | SelectProbes of (Probe.t * Instr_idx.t) list
    (* Should include all but the branch just taken *)
    | SelectProbes_AssertStable of
        (* should be true *) Probe.t * (* should be false *) Probe.t list
    (* These are ``magic'' instructions that allow user io operations. These
       instruction should be placed immediatly after the assoiated send/read
       instruction *)
    | Send_enqueuer of Enqueuer_idx.t
    | Read_dequeuer of Dequeuer_idx.t
  [@@deriving sexp_of]
end

module E : sig
  type t =
    | Uninit_id of Var_id.t * Instr_idx.t
    | Simul_read_write_var of Instr_idx.t * Instr_idx.t * Var_id.t
    | Simul_write_write_var of Instr_idx.t * Instr_idx.t * Var_id.t
    | Select_no_guards_true of Instr_idx.t
    | Select_multiple_guards_true of Instr_idx.t * int list
    | Assert_failure of Instr_idx.t * CInt.t
    | Simul_chan_readers of Instr_idx.t * Instr_idx.t
    | Simul_chan_senders of Instr_idx.t * Instr_idx.t
    | Select_multiple_true_probes of Instr_idx.t * (int * (Probe.t * int)) list
    | Unstable_probe of Instr_idx.t * Probe.t
    | Read_dequeuer_wrong_value of Dequeuer_idx.t * CInt.t * int
    | Mem_out_of_bounds of Instr_idx.t * CInt.t * int
    | User_read_did_not_complete of Dequeuer_idx.t * int
    | User_send_did_not_complete of Enqueuer_idx.t * int
    | Stuck
    | Time_out
  [@@deriving sexp_of]
end

module Var_spec : sig
  type t = { bitwidth : int }
end

module Chan_spec : sig
  type t = { bitwidth : int }
end

module Mem_spec : sig
  type t = {
    cell_bitwidth : int;
    idx_helper_reg : Var_id.t;
    init : CInt.t array;
  }
end

module Enqueuer_spec : sig
  type t = { var_id : Var_id.t }
end

module Dequeuer_spec : sig
  type t = { var_id : Var_id.t }
end

type t [@@deriving sexp_of]

val create :
  assem:N.t array ->
  assem_guard_read_ids:Var_id.Set.t array ->
  assem_guard_write_ids:Var_id.Set.t array ->
  vars:Var_spec.t array ->
  chans:Chan_spec.t array ->
  mems:Mem_spec.t array ->
  enqueuers:Enqueuer_spec.t array ->
  dequeuers:Dequeuer_spec.t array ->
  seed:int ->
  t

val set_enqueuer :
  t ->
  enqueuer_idx:Enqueuer_idx.t ->
  is_done:bool ->
  idx:int ->
  to_send:CInt.t array ->
  push_pc:Instr_idx.t ->
  unit

val set_dequeuer :
  t ->
  dequeuer_idx:Dequeuer_idx.t ->
  idx:int ->
  expected_reads:CInt.t array ->
  push_pc:Instr_idx.t ->
  unit

val wait : t -> max_steps:int -> unit -> (Instr_idx.t * CInt.t) Vec.t * E.t
val reset : t -> unit
