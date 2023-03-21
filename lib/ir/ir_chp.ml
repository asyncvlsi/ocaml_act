open! Core

module Chan_end = struct
  type t = Read of Ir_chan.U.t | Send of Ir_chan.U.t [@@deriving sexp_of]
end

type t =
  | Nop
  | Assign of Code_pos.t * Ir_var.U.t * Ir_expr.U.t
  | Log of Code_pos.t * string
  | Log1 of Code_pos.t * Ir_expr.U.t * (Any.t -> string)
  | Assert of Code_pos.t * Cbool0.t Ir_expr.t
  | Seq of Code_pos.t * t list
  | Par of Code_pos.t * t list
  | Read of Code_pos.t * Ir_chan.U.t * Ir_var.U.t
  | Send of Code_pos.t * Ir_chan.U.t * Ir_expr.U.t
  | Loop of Code_pos.t * t
  | WhileLoop of Code_pos.t * Cbool0.t Ir_expr.t * t
  | DoWhile of Code_pos.t * t * Cbool0.t Ir_expr.t
  | SelectImm of Code_pos.t * (Cbool0.t Ir_expr.t * t) list * t option
  | ReadUGMem of Code_pos.t * Ir_mem.t * Cint0.t Ir_expr.t * Ir_var.U.t
  | WriteUGMem of Code_pos.t * Ir_mem.t * Cint0.t Ir_expr.t * Ir_expr.U.t
  | WaitUntilReadReady of Code_pos.t * Ir_chan.U.t
  | WaitUntilSendReady of Code_pos.t * Ir_chan.U.t
  | Nondeterm_select of Code_pos.t * (Chan_end.t * t) list
[@@deriving sexp_of]
