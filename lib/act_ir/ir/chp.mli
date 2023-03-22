open! Core

module Chan_end : sig
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

type sexper = Cint.t -> Sexp.t [@@deriving sexp_of]
type expr = Var.t Expr.t [@@deriving sexp_of]
type bool_expr = Var.t Expr.t [@@deriving sexp_of]
type cp = Utils.Code_pos.t [@@deriving sexp_of]

(** @canonical Act_ir.Ir.Chp.t *)
type t =
  | Nop
  | Assign of cp * (Var.t * sexper) * expr
  | Log of cp * string
  | Log1 of cp * expr * (Cint.t -> string)
  | Assert of cp * expr
  | Seq of cp * t list
  | Par of cp * t list
  | Read of cp * Chan.t * (Var.t * sexper)
  | Send of cp * (Chan.t * sexper) * expr
  | Loop of cp * t
  | WhileLoop of cp * bool_expr * t
  | DoWhile of cp * t * bool_expr
  | SelectImm of cp * (bool_expr * t) list * t option
  | ReadUGMem of cp * Mem.t * expr * (Var.t * sexper)
  | WriteUGMem of cp * (Mem.t * sexper) * expr * expr
  | WaitUntilReadReady of cp * Chan.t
  | WaitUntilSendReady of cp * Chan.t
  | Nondeterm_select of cp * (Chan_end.t * t) list
[@@deriving sexp_of]

val assign : Var.t -> expr -> t
val read : Chan.t -> Var.t -> t
val send : Chan.t -> expr -> t
val send_var : Chan.t -> Var.t -> t

(* interacting with memories *)
val read_mem : Mem.t -> idx:expr -> dst:Var.t -> t
val write_mem : Mem.t -> idx:expr -> value:expr -> t

(* phantom instructions *)
val log : string -> t
val assert_ : expr -> t

(* control flow *)
val seq : t list -> t
val par : t list -> t
val if_else : expr -> t list -> t list -> t
val loop : t list -> t
val while_loop : expr -> t list -> t
val select_imm : (expr * t) list -> else_:t option -> t
