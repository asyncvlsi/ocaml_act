open! Core

module T = struct
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
end

include T

let assign ?loc var_id expr =
  Assign
    (Code_pos.value_or_psite loc, Var.Ir.untype' var_id, Expr.Ir.untype' expr)

let toggle ?loc var_id = assign ?loc var_id Expr.(var var_id |> not_)
let incr ?loc var_id = assign ?loc var_id Expr.(var var_id |> add (const 1))

let read ?loc chan_id var_id =
  Read
    ( Code_pos.value_or_psite loc,
      Chan.Ir.unwrap_r chan_id,
      Var.Ir.untype' var_id )

let send ?loc chan_id expr =
  Send
    (Code_pos.value_or_psite loc, Chan.Ir.unwrap_w chan_id, Expr.Ir.untype' expr)

let send' ?loc chan_id var_id = send ?loc chan_id Expr.(var var_id)

(* interacting with memories *)
let read_ug_mem ?loc (mem : 'a Mem.ug_mem) ~idx ~(dst : 'a Var.t) =
  ReadUGMem
    ( Code_pos.value_or_psite loc,
      Mem.Ir.unwrap_ug_mem mem,
      Expr.Ir.unwrap idx,
      Var.Ir.untype' dst )

let write_ug_mem ?loc (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Expr.t) =
  WriteUGMem
    ( Code_pos.value_or_psite loc,
      Mem.Ir.unwrap_ug_mem mem,
      Expr.Ir.unwrap idx,
      Expr.Ir.untype' value )

let write_ug_mem' ?loc (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Var.t) =
  write_ug_mem ?loc mem ~idx ~value:Expr.(var value)

let read_ug_rom ?loc (rom : 'a Mem.ug_rom) ~idx ~(dst : 'a Var.t) =
  ReadUGMem
    ( Code_pos.value_or_psite loc,
      Mem.Ir.unwrap_ug_rom rom,
      Expr.Ir.unwrap idx,
      Var.Ir.untype' dst )

let log ?loc expr = Log (Code_pos.value_or_psite loc, Expr.Ir.unwrap expr)
let assert_ ?loc expr = Assert (Code_pos.value_or_psite loc, Expr.Ir.unwrap expr)
let seq ?loc l = Seq (Code_pos.value_or_psite loc, l)
let par ?loc l = Par (Code_pos.value_or_psite loc, l)

let if_else ?loc expr t_br f_br =
  SelectImm
    ( Code_pos.value_or_psite loc,
      [ (Expr.Ir.unwrap expr, seq ?loc t_br) ],
      Some (seq ?loc f_br) )

let loop ?loc t = Loop (Code_pos.value_or_psite loc, seq ?loc t)

let while_loop ?loc expr t =
  WhileLoop (Code_pos.value_or_psite loc, Expr.Ir.unwrap expr, seq ?loc t)

let select_imm ?loc branches ~else_ =
  SelectImm
    ( Code_pos.value_or_psite loc,
      List.map branches ~f:(fun (guard, stmt) -> (Expr.Ir.unwrap guard, stmt)),
      else_ )

module Ir = struct
  include T

  type outer = t

  let unwrap t = t
end
