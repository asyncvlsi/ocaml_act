open! Core
open! Act

module Var : sig
  type t [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Chan : sig
  type t [@@deriving sexp, hash, equal, compare]

  include Comparable.S with type t := t
  include Hashable.S with type t := t
end

module Expr : sig
  type 'v t =
    | Var of 'v
    | Const of CInt.t
    | Add of 'v t * 'v t
    | Sub_no_wrap of 'v t * 'v t
    | Mul of 'v t * 'v t
    | Div of 'v t * 'v t
    | Mod of 'v t * 'v t
    | Eq of 'v t * 'v t
    | Ne of 'v t * 'v t
    | Gt of 'v t * 'v t
    | Ge of 'v t * 'v t
    | Lt of 'v t * 'v t
    | Le of 'v t * 'v t
    | BitXor of 'v t * 'v t
    | BitOr of 'v t * 'v t
    | BitAnd of 'v t * 'v t
    | LShift of 'v t * 'v t
    | RShift of 'v t * 'v t
    | Clip of 'v t * int
  [@@deriving sexp]

  val map_vars : 'v t -> f:('v -> 'u) -> 'u t
  val var_ids : 'v t -> 'v list
  val bitwidth : 'v t -> bits_of_var:('v -> int) -> int
end

module Chp_stmt : sig
  type t =
    | Nop
    | Assign of Var.t * Var.t Expr.t
    | Seq of t list
    | Par of t list
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t Expr.t
    | DoWhile of t * Var.t Expr.t
      (* This expr is a one-hot vector with List.length branches bits
          indexing into the list of branches *)
    | SelectImm of Var.t Expr.t * t list
  [@@deriving sexp_of]
end

module Chp_proc : sig
  type t = Chp_stmt.t [@@deriving sexp_of]
end

module Mem_proc : sig
  type t = {
    init : CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Chan.t;
    read_chan : Chan.t;
    write_chan : Chan.t;
  }
  [@@deriving sexp_of]
end

module Proc : sig
  module K : sig
    type t =
      | Chp of Chp_proc.t
      | Mem of Mem_proc.t
      | INode of Chan.t * Act.Internal_rep.Chan.U.t
      | ONode of Chan.t * Act.Internal_rep.Chan.U.t
    [@@deriving sexp_of]
  end

  type t = { k : K.t; in_chans : Chan.Set.t; out_chans : Chan.Set.t }
end

val of_ir :
  Act.Chp.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  Proc.t list Or_error.t
