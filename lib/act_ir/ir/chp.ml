open! Core

type sexper = Cint.t -> Sexp.t [@@deriving sexp_of]
type expr = Var.t Expr.t [@@deriving sexp_of]
type bool_expr = Var.t Expr.t [@@deriving sexp_of]
type cp = Code_pos.t [@@deriving sexp_of]

module M = struct
  type t = {
    cp : Utils.Code_pos.t;
    var_sexper : Cint.t -> Sexp.t;
    chan_sexper : Cint.t -> Sexp.t;
    cell_sexper : Cint.t -> Sexp.t;
  }
  [@@deriving sexp_of]

  let create ?(var_sexper = Cint.sexp_of_t) ?(chan_sexper = Cint.sexp_of_t)
      ?(cell_sexper = Cint.sexp_of_t) cp =
    { cp; var_sexper; chan_sexper; cell_sexper }

  let none = create Utils.Code_pos.dummy_loc
end

module Log1 = struct
  type t = { m : M.t; f : Cint.t -> string; expr : Var.t Expr.t }
  [@@deriving sexp_of]
end

module Assert = struct
  type t = { m : M.t; expr : Var.t Expr.t } [@@deriving sexp_of]
end

module Assign = struct
  type t = { m : M.t; var : Var.t; expr : Var.t Expr.t } [@@deriving sexp_of]
end

module Read = struct
  type t = { m : M.t; chan : Chan.t; var : Var.t } [@@deriving sexp_of]
end

module Send = struct
  type t = { m : M.t; chan : Chan.t; expr : Var.t Expr.t } [@@deriving sexp_of]
end

module ReadMem = struct
  type t = { m : M.t; mem : Mem.t; idx : Var.t Expr.t; var : Var.t }
  [@@deriving sexp_of]
end

module WriteMem = struct
  type t = { m : M.t; mem : Mem.t; idx : Var.t Expr.t; expr : Var.t Expr.t }
  [@@deriving sexp_of]
end

module Seq = struct
  type 'n t = { m : M.t; ns : 'n list } [@@deriving sexp_of]
end

module Par = struct
  type 'n t = { m : M.t; ns : 'n list } [@@deriving sexp_of]
end

module WhileLoop = struct
  type 'n t = { m : M.t; g : Var.t Expr.t; n : 'n } [@@deriving sexp_of]
end

module DoWhile = struct
  type 'n t = { m : M.t; n : 'n; g : Var.t Expr.t } [@@deriving sexp_of]
end

module SelectImm = struct
  type 'n t = {
    m : M.t;
    branches : (Var.t Expr.t * 'n) list;
    else_ : 'n option;
  }
  [@@deriving sexp_of]
end

module Chan_end = struct
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

module Nondeterm_select = struct
  type 'n t = { m : M.t; branches : (Chan_end.t * 'n) list }
  [@@deriving sexp_of]
end

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

let nop ?(m = M.none) () = Nop m
let assign ?(m = M.none) var expr = Assign { m; var; expr }
let read ?(m = M.none) chan var = Read { m; chan; var }
let send ?(m = M.none) chan expr = Send { m; chan; expr }
let send_var ?m chan var = send ?m chan (Var var)
let read_mem ?(m = M.none) mem ~idx ~dst = ReadMem { m; mem; idx; var = dst }

let write_mem ?(m = M.none) mem ~idx ~value =
  WriteMem { m; mem; idx; expr = value }

let log1 ?(m = M.none) expr ~f = Log1 { m; expr; f }
let log ?m str = log1 ?m (Const Cint.zero) ~f:(fun _ -> str)
let assert_ ?(m = M.none) expr = Assert { m; expr }

(* control flow *)
let seq ?(m = M.none) ns = Seq { m; ns }
let par ?(m = M.none) ns = Par { m; ns }
let while_loop' ?(m = M.none) g n = WhileLoop { m; g; n }
let do_while' ?(m = M.none) n g = DoWhile { m; n; g }
let select_imm ?(m = M.none) branches ~else_ = SelectImm { m; branches; else_ }
let nondeterm_select ?(m = M.none) branches = Nondeterm_select { m; branches }
let while_loop ?m g ns = while_loop' ?m g (seq ?m ns)
let do_while ?m ns g = do_while' ?m (seq ?m ns) g
let loop ?m ns = do_while ?m ns (Const Cint.one)

let if_else ?m e ns_true ns_false =
  select_imm ?m [ (e, seq ?m ns_true) ] ~else_:(Some (seq ?m ns_false))
