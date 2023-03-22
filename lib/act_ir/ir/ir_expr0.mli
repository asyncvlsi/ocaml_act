open! Core

(* This module is unsafe, and is only meant for compiler use. It is needed to
   break a dependency cycle from Var -> Dtype -> Expr -> Var *)

type 'v t =
  | Var of 'v
  | Const of Cint.t
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
      * (Cint.t -> string)
[@@deriving sexp_of]

(* ops *)
val var : 'v -> 'v t
val of_int : int -> 'v t
val of_cint : Cint.t -> 'v t
val add : 'v t -> 'v t -> 'v t
val sub : 'v t -> 'v t -> 'v t
val mul : 'v t -> 'v t -> 'v t
val div : 'v t -> 'v t -> 'v t
val mod_ : 'v t -> 'v t -> 'v t
val left_shift : 'v t -> amt:'v t -> 'v t
val right_shift : 'v t -> amt:'v t -> 'v t
val left_shift' : 'v t -> amt:int -> 'v t
val right_shift' : 'v t -> amt:int -> 'v t
val bit_and : 'v t -> 'v t -> 'v t
val bit_or : 'v t -> 'v t -> 'v t
val bit_xor : 'v t -> 'v t -> 'v t
val eq : 'v t -> 'v t -> 'v t
val ne : 'v t -> 'v t -> 'v t
val lt : 'v t -> 'v t -> 'v t
val le : 'v t -> 'v t -> 'v t
val gt : 'v t -> 'v t -> 'v t
val ge : 'v t -> 'v t -> 'v t
val clip : 'v t -> bits:int -> 'v t
val not_ : 'v t -> 'v t
val and_ : 'v t -> 'v t -> 'v t
val or_ : 'v t -> 'v t -> 'v t
val xor_ : 'v t -> 'v t -> 'v t
val zero : 'v t
val one : 'v t
val two : 'v t
val three : 'v t
val four : 'v t
val five : 'v t
val true_ : 'v t
val false_ : 'v t
