open! Core

module Chan_end : sig
  type t = Read of Ir_chan.t | Send of Ir_chan.t [@@deriving sexp_of]
end

type sexper = Cint.t -> Sexp.t [@@deriving sexp_of]
type expr = Ir_var.t Ir_expr0.t [@@deriving sexp_of]
type bool_expr = Ir_var.t Ir_expr0.t [@@deriving sexp_of]
type cp = Utils.Code_pos.t [@@deriving sexp_of]

type t =
  | Nop
  | Assign of cp * (Ir_var.t * sexper) * expr
  | Log of cp * string
  | Log1 of cp * expr * (Cint.t -> string)
  | Assert of cp * expr
  | Seq of cp * t list
  | Par of cp * t list
  | Read of cp * Ir_chan.t * (Ir_var.t * sexper)
  | Send of cp * (Ir_chan.t * sexper) * expr
  | Loop of cp * t
  | WhileLoop of cp * bool_expr * t
  | DoWhile of cp * t * bool_expr
  | SelectImm of cp * (bool_expr * t) list * t option
  | ReadUGMem of cp * Ir_mem.t * expr * (Ir_var.t * sexper)
  | WriteUGMem of cp * (Ir_mem.t * sexper) * expr * expr
  | WaitUntilReadReady of cp * Ir_chan.t
  | WaitUntilSendReady of cp * Ir_chan.t
  | Nondeterm_select of cp * (Chan_end.t * t) list
[@@deriving sexp_of]

val assign : Ir_var.t -> expr -> t
val read : Ir_chan.t -> Ir_var.t -> t
val send : Ir_chan.t -> expr -> t
val send_var : Ir_chan.t -> Ir_var.t -> t

(* interacting with memories *)
val read_mem : Ir_mem.t -> idx:expr -> dst:Ir_var.t -> t
val write_mem : Ir_mem.t -> idx:expr -> value:expr -> t

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
