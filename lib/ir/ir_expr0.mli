open! Core

(* This module is unsafe, and is only meant for compiler use. It is needed to
   break a dependency cycle from Var -> DType -> Expr -> Var *)

type 'v t =
  | Var of 'v
  | Const of Cint0.t
  | Add of 'v t * 'v t
  | Sub_no_wrap of 'v t * 'v t
  | Sub_wrap of 'v t * 'v t * int
  | Mul of 'v t * 'v t
  | Div of 'v t * 'v t
  | Mod of 'v t * 'v t
  | LShift of 'v t * 'v t
  | LogicalRShift of 'v t * 'v t
  | BitAnd of 'v t * 'v t
  | BitOr of 'v t * 'v t
  | BitXor of 'v t * 'v t
  | Eq of 'v t * 'v t
  | Ne of 'v t * 'v t
  | Lt of 'v t * 'v t
  | Le of 'v t * 'v t
  | Gt of 'v t * 'v t
  | Ge of 'v t * 'v t
  | Clip of 'v t * int
  (* This asserts that the first expression (which must have value 0 or 1) is 1,
     and then returns the second value. In the simulator, if it is false, it
     calls the function for a nice error report. *)
  | With_assert_log of
      (* assert *)
      'v t
      * (* value *)
      'v t
      * (* log_input *)
      'v t
      * (Cint0.t -> string)
[@@deriving sexp_of]
