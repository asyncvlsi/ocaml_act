open! Core

module Chp : sig
  module Var : sig
    module Id : sig
      include Identifiable

      val to_int : t -> int
    end

    type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
    [@@deriving sexp, hash, equal, compare]

    include Comparable with type t := t
    include Hashable with type t := t
  end

  module Chan : sig
    module Id : sig
      include Identifiable

      val to_int : t -> int
    end

    type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
    [@@deriving sexp, hash, equal, compare]

    include Comparable with type t := t
    include Hashable with type t := t
  end

  module Assert : sig
    module Err_msg : sig
      type t =
        | Eval_expr_failed of (Act.Code_pos.t * (Act.CInt.t -> string))
        | Assert_failed of Act.Code_pos.t
        | Value_doesnt_fit of (Act.Code_pos.t * [ `Var | `Chan | `Mem_cell ])
        | Idx_out_of_bounds of (Act.Code_pos.t * int)
      [@@deriving sexp_of]
    end

    type t = { cond : Var.t Expr.t; log_e : Var.t Expr.t; msg : Err_msg.t }
    [@@deriving sexp_of]
  end

  module Stmt : sig
    type t =
      | Nop
      | Log of Act.Code_pos.t * Var.t Expr.t * (Act.CInt.t -> string)
      | Assert of Assert.t
      | Assign of Act.Code_pos.t * Var.t * Var.t Expr.t
      | Seq of t list
      | Par of t list
      (* assert happens immediatly after read before any other code runs *)
      | ReadThenAssert of Act.Code_pos.t * Chan.t * Var.t * Assert.t
      | Send of Act.Code_pos.t * Chan.t * Var.t Expr.t
      | DoWhile of Act.Code_pos.t * t * Var.t Expr.t
        (* This expr is a one-hot vector with List.length branches bits
            indexing into the list of branches *)
      | SelectImm of Act.Code_pos.t * Var.t Expr.t * t list
    [@@deriving sexp_of]
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
end

module Mem_proc : sig
  type t = {
    init : Act.CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Interproc_chan.t;
    read_chan : Interproc_chan.t;
    write_chan : Interproc_chan.t option;
  }
  [@@deriving sexp_of]
end

module Process : sig
  module K : sig
    type t = Chp of Chp.Proc.t | Mem of Mem_proc.t [@@deriving sexp_of]
  end

  type t = { k : K.t }
end

module Program : sig
  type t = {
    processes : Process.t list;
    top_iports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
    top_oports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
  }
  [@@deriving sexp_of]
end

val of_program :
  Act.Program.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  Program.t
