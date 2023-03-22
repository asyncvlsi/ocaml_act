open! Core

type t [@@deriving sexp_of]

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
val read_ug_mem : 'a Mem.ug_mem -> idx:Act_ir.CInt.t Expr.t -> dst:'a Var.t -> t

val write_ug_mem :
  'a Mem.ug_mem -> idx:Act_ir.CInt.t Expr.t -> value:'a Expr.t -> t

val write_ug_mem' :
  'a Mem.ug_mem -> idx:Act_ir.CInt.t Expr.t -> value:'a Var.t -> t

val read_ug_rom : 'a Mem.ug_rom -> idx:Act_ir.CInt.t Expr.t -> dst:'a Var.t -> t

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

(**/**)

module Internal : sig
  val unwrap : t -> Act_ir.Ir.Chp.t
  val wrap : Act_ir.Ir.Chp.t -> t
end

(**/**)
