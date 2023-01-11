open! Core

type t

val assign : ?loc:Code_pos.t -> 'a Var.t -> 'a Expr.t -> t
val toggle : ?loc:Code_pos.t -> bool Var.t -> t
val incr : ?loc:Code_pos.t -> int Var.t -> t
val read : ?loc:Code_pos.t -> 'a Chan.R.t -> 'a Var.t -> t
val send : ?loc:Code_pos.t -> 'a Chan.W.t -> 'a Expr.t -> t
val send' : ?loc:Code_pos.t -> 'a Chan.W.t -> 'a Var.t -> t

(* probes *)
val wait_probe_r : ?loc:Code_pos.t -> 'a Chan.R.t -> t
val wait_probe_w : ?loc:Code_pos.t -> 'a Chan.W.t -> t

(* interacting with memories *)
val read_ug_mem :
  ?loc:Code_pos.t -> 'a Mem.ug_mem -> idx:int Expr.t -> dst:'a Var.t -> t

val write_ug_mem :
  ?loc:Code_pos.t -> 'a Mem.ug_mem -> idx:int Expr.t -> value:'a Expr.t -> t

val write_ug_mem' :
  ?loc:Code_pos.t -> 'a Mem.ug_mem -> idx:int Expr.t -> value:'a Var.t -> t

val read_ug_rom :
  ?loc:Code_pos.t -> 'a Mem.ug_rom -> idx:int Expr.t -> dst:'a Var.t -> t

(* phantom instructions *)
val log : ?loc:Code_pos.t -> string Expr.t -> t
val assert_ : ?loc:Code_pos.t -> bool Expr.t -> t

(* control flow *)
val seq : ?loc:Code_pos.t -> t list -> t
val par : ?loc:Code_pos.t -> t list -> t
val if_else : ?loc:Code_pos.t -> bool Expr.t -> t list -> t list -> t
val loop : ?loc:Code_pos.t -> t list -> t
val while_loop : ?loc:Code_pos.t -> bool Expr.t -> t list -> t

val select_imm :
  ?loc:Code_pos.t -> (bool Expr.t * t) list -> else_:t option -> t

module Ir : sig
  type outer = t

  type t =
    | Assign of Code_pos.t * Var.Ir.U.t * Expr.Ir.U.t
    | Log of Code_pos.t * string Expr.Ir.t
    | Assert of Code_pos.t * bool Expr.Ir.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | Read of Code_pos.t * Chan.Ir.U.t * Var.Ir.U.t
    | Send of Code_pos.t * Chan.Ir.U.t * Expr.Ir.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * bool Expr.Ir.t * t
    | SelectImm of Code_pos.t * (bool Expr.Ir.t * t) list * t option
    | ReadUGMem of Code_pos.t * Mem.Ir.t * int Expr.Ir.t * Var.Ir.U.t
    | WriteUGMem of Code_pos.t * Mem.Ir.t * int Expr.Ir.t * Expr.Ir.U.t
    | WaitUntilReadReady of Code_pos.t * Chan.Ir.U.t
    | WaitUntilSendReady of Code_pos.t * Chan.Ir.U.t

  val unwrap : outer -> t
end
