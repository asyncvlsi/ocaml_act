open! Core

module Wrap : sig
  type t

  val assign : ?loc:Code_pos.t -> 'a Var.Wrap.t -> 'a Expr.Wrap.t -> t
  val read : ?loc:Code_pos.t -> 'a Chan.Wrap.R.t -> 'a Var.Wrap.t -> t
  val send : ?loc:Code_pos.t -> 'a Chan.Wrap.W.t -> 'a Expr.Wrap.t -> t
  val send' : ?loc:Code_pos.t -> 'a Chan.Wrap.W.t -> 'a Var.Wrap.t -> t

  (* probes *)
  val wait_probe_r : ?loc:Code_pos.t -> 'a Chan.Wrap.R.t -> t
  val wait_probe_w : ?loc:Code_pos.t -> 'a Chan.Wrap.W.t -> t
  val select_probe_r : ?loc:Code_pos.t -> Chan.Wrap.R.U.t list -> t
  val select_probe_w : ?loc:Code_pos.t -> Chan.Wrap.R.U.t list -> t

  val select_probe :
    ?loc:Code_pos.t ->
    [ `Read of Chan.Wrap.R.U.t | `Send of Chan.Wrap.W.U.t ] list ->
    t

  (* interacting with memories *)
  val read_ug_mem :
    ?loc:Code_pos.t ->
    'a Mem.Wrap.ug_mem ->
    idx:Cint0.t Expr.Wrap.t ->
    dst:'a Var.Wrap.t ->
    t

  val write_ug_mem :
    ?loc:Code_pos.t ->
    'a Mem.Wrap.ug_mem ->
    idx:Cint0.t Expr.Wrap.t ->
    value:'a Expr.Wrap.t ->
    t

  val write_ug_mem' :
    ?loc:Code_pos.t ->
    'a Mem.Wrap.ug_mem ->
    idx:Cint0.t Expr.Wrap.t ->
    value:'a Var.Wrap.t ->
    t

  val read_ug_rom :
    ?loc:Code_pos.t ->
    'a Mem.Wrap.ug_rom ->
    idx:Cint0.t Expr.Wrap.t ->
    dst:'a Var.Wrap.t ->
    t

  (* phantom instructions *)
  val log : ?loc:Code_pos.t -> string -> t
  val log1 : ?loc:Code_pos.t -> 'a Var.Wrap.t -> f:('a -> string) -> t
  val log1' : ?loc:Code_pos.t -> 'a Expr.Wrap.t -> f:('a -> string) -> t
  val assert_ : ?loc:Code_pos.t -> Cbool0.t Expr.Wrap.t -> t

  (* control flow *)
  val seq : ?loc:Code_pos.t -> t list -> t
  val par : ?loc:Code_pos.t -> t list -> t
  val if_else : ?loc:Code_pos.t -> Cbool0.t Expr.Wrap.t -> t list -> t list -> t
  val loop : ?loc:Code_pos.t -> t list -> t
  val while_loop : ?loc:Code_pos.t -> Cbool0.t Expr.Wrap.t -> t list -> t

  val select_imm :
    ?loc:Code_pos.t -> (Cbool0.t Expr.Wrap.t * t) list -> else_:t option -> t
end

module Ir : sig
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
    | SelectImm of Code_pos.t * (Cbool0.t Expr.Ir.t * t) list * t option
    | ReadUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Var.Ir.U.t
    | WriteUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Expr.Ir.U.t
    | WaitUntilReadReady of Code_pos.t * Chan.Ir.U.t
    | WaitUntilSendReady of Code_pos.t * Chan.Ir.U.t

  val unwrap : Wrap.t -> t

  module No_width_checks : sig
    val assign : ?loc:Code_pos.t -> 'a Var.Ir.t -> 'a Expr.Ir.t -> Wrap.t
    val read : ?loc:Code_pos.t -> Chan.Ir.U.t -> 'a Var.Ir.t -> Wrap.t
    val send : ?loc:Code_pos.t -> Chan.Ir.U.t -> 'a Expr.Ir.t -> Wrap.t
  end
end
