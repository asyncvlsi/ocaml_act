open! Core

module Code_pos : sig
  type t = {
    filename : string;
    line_number : int;
    start_char : int;
    end_char : int;
  }
  [@@deriving sexp]
end

module DType : sig
  type 'a t

  val int_ : int t
end

module Chan : sig
  module U : sig
    type t [@@deriving sexp_of, compare, equal, hash]

    include Comparable with type t := t
    include Hashable with type t := t
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : 'a DType.t -> 'a t
end

module Var : sig
  module U : sig
    type t [@@deriving sexp_of]
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : 'a DType.t -> 'a t
end

module Expr : sig
  type 'a t =
    | Var of 'a Var.t
    | Const of Any.t
    | Map of Any.t t * (Any.t -> 'a)
  [@@deriving sexp_of]

  val const : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module N : sig
  type t

  val assign : 'a Var.t -> 'a Expr.t -> t
  val log : string Expr.t -> t
  val assert_ : bool Expr.t -> t
  val seq : t list -> t
  val par : t list -> t
  val if_else : bool Expr.t -> t -> t -> t
  val read : 'a Chan.t -> 'a Var.t -> t
  val send : 'a Chan.t -> 'a Expr.t -> t
  val loop : t -> t
  val while_loop : bool Expr.t -> t -> t
end

module Sim : sig
  module Wait_outcome : sig
    type t =
      | Already_errored
      | Time_out
      | Stuck
      | Assert_failure of Code_pos.t
      | Uninit_id of Var.U.t * Code_pos.t
      | Simul_chan_senders of Code_pos.t * Code_pos.t
      | Simul_chan_readers of Code_pos.t * Code_pos.t
      | Simul_read_write of Var.U.t * Code_pos.t * Code_pos.t
      | Simul_write_write of Var.U.t * Code_pos.t * Code_pos.t
      | Select_no_guards_true of Code_pos.t
      | Select_multiple_guards_true of int list * Code_pos.t
      | User_read_failed_to_complete of Chan.U.t
      | User_write_failed_to_complete of Chan.U.t
      | User_read_incorrect_value of Chan.U.t
      | Read_dequeuer_wrong_value of Chan.U.t * Sexp.t * Sexp.t
      | Read_dequeuer_not_done of Chan.U.t
      | Send_enqueuer_not_done of Chan.U.t
    [@@deriving sexp]
  end

  type t

  val create :
    N.t ->
    user_sendable_ports:Chan.U.Set.t ->
    user_readable_ports:Chan.U.Set.t ->
    t

  val wait : ?max_steps:int -> t -> Wait_outcome.t
  val wait' : ?max_steps:int -> t -> unit
  val send : t -> 'a Chan.t -> 'a -> unit
  val read : t -> 'a Chan.t -> 'a -> unit
end
