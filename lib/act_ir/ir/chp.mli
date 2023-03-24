open! Core
open Utils

module M : sig
  (* By "default" these should be Utils.Code_pos.dummy_pos and Cint.sexp_of_t *)
  type t = {
    cp : Utils.Code_pos.t;
    var_sexper : CInt.t -> Sexp.t;
    chan_sexper : CInt.t -> Sexp.t;
    cell_sexper : CInt.t -> Sexp.t;
  }
  [@@deriving sexp_of]

  val none : t

  val create :
    ?var_sexper:(CInt.t -> Sexp.t) ->
    ?chan_sexper:(CInt.t -> Sexp.t) ->
    ?cell_sexper:(CInt.t -> Sexp.t) ->
    Utils.Code_pos.t ->
    t
end

module Log1 : sig
  type t = { m : M.t; f : CInt.t -> string; expr : Var.t Expr.t }
  [@@deriving sexp_of]
end

module Assert : sig
  type t = {
    m : M.t;
    expr : Var.t Expr.t;
    log_e : Var.t Expr.t;
    msg_fn : CInt.t -> string;
  }
  [@@deriving sexp_of]
end

module Assign : sig
  type t = { m : M.t; var : Var.t; expr : Var.t Expr.t } [@@deriving sexp_of]
end

module Read : sig
  type t = { m : M.t; chan : Chan.t; var : Var.t } [@@deriving sexp_of]
end

module Send : sig
  type t = { m : M.t; chan : Chan.t; expr : Var.t Expr.t } [@@deriving sexp_of]
end

module ReadMem : sig
  type t = { m : M.t; mem : Mem.t; idx : Var.t Expr.t; var : Var.t }
  [@@deriving sexp_of]
end

module WriteMem : sig
  type t = { m : M.t; mem : Mem.t; idx : Var.t Expr.t; expr : Var.t Expr.t }
  [@@deriving sexp_of]
end

module Seq : sig
  type 'n t = { m : M.t; ns : 'n list } [@@deriving sexp_of]
end

module Par : sig
  type 'n t = { m : M.t; ns : 'n list } [@@deriving sexp_of]
end

module WhileLoop : sig
  type 'n t = { m : M.t; g : Var.t Expr.t; n : 'n } [@@deriving sexp_of]
end

module DoWhile : sig
  type 'n t = { m : M.t; n : 'n; g : Var.t Expr.t } [@@deriving sexp_of]
end

module SelectImm : sig
  type 'n t = {
    m : M.t;
    branches : (Var.t Expr.t * 'n) list;
    else_ : 'n option;
  }
  [@@deriving sexp_of]
end

module Chan_end : sig
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

module Nondeterm_select : sig
  type 'n t = { m : M.t; branches : (Chan_end.t * 'n) list }
  [@@deriving sexp_of]
end

(** @canonical Act_ir.Ir.Chp.t *)
type t =
  | Nop of M.t
  | Log1 of Log1.t
  | Assert of Assert.t
  | Assign of Assign.t
  | Read of Read.t
  | Send of Send.t
  | ReadMem of ReadMem.t
  | WriteMem of WriteMem.t
  | Seq of t Seq.t
  | Par of t Par.t
  | WhileLoop of t WhileLoop.t
  | DoWhile of t DoWhile.t
  | SelectImm of t SelectImm.t
  | Nondeterm_select of t Nondeterm_select.t
[@@deriving sexp_of]

val nop : ?m:M.t -> unit -> t
val assign : ?m:M.t -> Var.t -> Var.t Expr.t -> t
val read : ?m:M.t -> Chan.t -> Var.t -> t
val send : ?m:M.t -> Chan.t -> Var.t Expr.t -> t
val send_var : ?m:M.t -> Chan.t -> Var.t -> t

(* interacting with memories *)
val read_mem : ?m:M.t -> Mem.t -> idx:Var.t Expr.t -> dst:Var.t -> t
val write_mem : ?m:M.t -> Mem.t -> idx:Var.t Expr.t -> value:Var.t Expr.t -> t

(* phantom instructions *)
val log : ?m:M.t -> string -> t
val log1 : ?m:M.t -> Var.t Expr.t -> f:(CInt.t -> string) -> t
val assert_ : ?m:M.t -> Var.t Expr.t -> t
val assert0 : ?m:M.t -> Var.t Expr.t -> string -> t

val assert1 :
  ?m:M.t -> Var.t Expr.t -> Var.t Expr.t -> f:(CInt.t -> string) -> t

(* control flow *)
val seq : ?m:M.t -> t list -> t
val par : ?m:M.t -> t list -> t
val if_else : ?m:M.t -> Var.t Expr.t -> t list -> t list -> t
val loop : ?m:M.t -> t list -> t
val while_loop : ?m:M.t -> Var.t Expr.t -> t list -> t
val do_while : ?m:M.t -> t list -> Var.t Expr.t -> t
val select_imm : ?m:M.t -> (Var.t Expr.t * t) list -> else_:t option -> t
val nondeterm_select : ?m:M.t -> (Chan_end.t * t) list -> t
