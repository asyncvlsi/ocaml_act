open! Core

type t

val assign : 'a Var.t -> 'a Expr.t -> t
val read : 'a Chan.R.t -> 'a Var.t -> t
val send : 'a Chan.W.t -> 'a Expr.t -> t
val send_var : 'a Chan.W.t -> 'a Var.t -> t

(* probes *)
val wait_probe_r : 'a Chan.R.t -> t
val wait_probe_w : 'a Chan.W.t -> t
val select_probe_r : Chan.R.U.t list -> t
val select_probe_w : Chan.R.U.t list -> t
val select_probe : [ `Read of Chan.R.U.t | `Send of Chan.W.U.t ] list -> t

(* interacting with memories *)
val read_ug_mem : 'a Mem.ug_mem -> idx:Cint0.t Expr.t -> dst:'a Var.t -> t
val write_ug_mem : 'a Mem.ug_mem -> idx:Cint0.t Expr.t -> value:'a Expr.t -> t
val write_ug_mem' : 'a Mem.ug_mem -> idx:Cint0.t Expr.t -> value:'a Var.t -> t
val read_ug_rom : 'a Mem.ug_rom -> idx:Cint0.t Expr.t -> dst:'a Var.t -> t

(* phantom instructions *)
val log : string -> t
val log1 : 'a Var.t -> f:('a -> string) -> t
val log1' : 'a Expr.t -> f:('a -> string) -> t
val assert_ : Cbool0.t Expr.t -> t

(* control flow *)
val seq : t list -> t
val par : t list -> t
val if_else : Cbool0.t Expr.t -> t list -> t list -> t
val loop : t list -> t
val while_loop : Cbool0.t Expr.t -> t list -> t
val select_imm : (Cbool0.t Expr.t * t) list -> else_:t option -> t

module Ir : sig
  type outer = t

  type t =
    | Nop
    | Assign of Code_pos.t * Var.Ir.U.t * Expr.Ir.U.t
    | Log of Code_pos.t * string
    | Log1 of Code_pos.t * Expr.Ir.U.t * (Any.t -> string)
    | Assert of Code_pos.t * Cbool0.t Expr.Ir.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | Read of Code_pos.t * Chan.Ir.U.t * Var.Ir.U.t
    | Send of Code_pos.t * Chan.Ir.U.t * Expr.Ir.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * Cbool0.t Expr.Ir.t * t
    | DoWhile of Code_pos.t * t * Cbool0.t Expr.Ir.t
    | SelectImm of Code_pos.t * (Cbool0.t Expr.Ir.t * t) list * t option
    | ReadUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Var.Ir.U.t
    | WriteUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Expr.Ir.U.t
    | WaitUntilReadReady of Code_pos.t * Chan.Ir.U.t
    | WaitUntilSendReady of Code_pos.t * Chan.Ir.U.t
  [@@deriving sexp_of]

  val unwrap : outer -> t
  val wrap : t -> outer

  module No_width_checks : sig
    val assign : loc:Code_pos.t -> 'a Var.Ir.t -> 'a Expr.Ir.t -> t
    val read : loc:Code_pos.t -> Chan.Ir.U.t -> 'a Var.Ir.t -> t
    val send : loc:Code_pos.t -> Chan.Ir.U.t -> 'a Expr.Ir.t -> t
  end
end
