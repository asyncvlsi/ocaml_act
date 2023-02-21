open! Core

type 'v t =
  | Var of 'v
  | Const of Act.CInt.t
  | Add of 'v t * 'v t
  | Sub_no_wrap of 'v t * 'v t
  | Mul of 'v t * 'v t
  | Div of 'v t * 'v t
  | Mod of 'v t * 'v t
  | Eq of 'v t * 'v t
  | Ne of 'v t * 'v t
  | Gt of 'v t * 'v t
  | Ge of 'v t * 'v t
  | Lt of 'v t * 'v t
  | Le of 'v t * 'v t
  | Eq0 of 'v t
  | BitXor of 'v t * 'v t
  | BitOr of 'v t * 'v t
  | BitAnd of 'v t * 'v t
  | LShift of 'v t * 'v t
  | RShift of 'v t * 'v t
  | Clip of 'v t * int
[@@deriving sexp]

val map_vars : 'v t -> f:('v -> 'u) -> 'u t
val var_ids : 'v t -> 'v list
val bitwidth : 'v t -> bits_of_var:('v -> int) -> int
