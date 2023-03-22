open! Core

module Chan_end = struct
  type t = Read of Ir_chan.t | Send of Ir_chan.t [@@deriving sexp_of]
end

type sexper = Cint.t -> Sexp.t [@@deriving sexp_of]
type expr = Ir_var.t Ir_expr0.t [@@deriving sexp_of]
type bool_expr = Ir_var.t Ir_expr0.t [@@deriving sexp_of]
type cp = Code_pos.t [@@deriving sexp_of]

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

let cp = Code_pos.dummy_loc
let assign v e = Assign (cp, (v, Cint.sexp_of_t), e)
let read c v = Read (cp, c, (v, Cint.sexp_of_t))
let send c e = Send (cp, (c, Cint.sexp_of_t), e)
let send_var c v = Send (cp, (c, Cint.sexp_of_t), Var v)
let read_mem mem ~idx ~dst = ReadUGMem (cp, mem, idx, (dst, Cint.sexp_of_t))

let write_mem mem ~idx ~value =
  WriteUGMem (cp, (mem, Cint.sexp_of_t), idx, value)

(* phantom instructions *)
let log s = Log (cp, s)
let assert_ e = Assert (cp, e)

(* control flow *)
let seq l = Seq (cp, l)
let par l = Par (cp, l)
let if_else e lt lf = SelectImm (cp, [ (e, Seq (cp, lt)) ], Some (Seq (cp, lf)))
let loop l = Loop (cp, Seq (cp, l))
let while_loop g l = WhileLoop (cp, g, Seq (cp, l))
let select_imm l ~else_ = SelectImm (cp, l, else_)
