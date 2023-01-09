open! Core

type 'a t [@@deriving sexp_of]

val var : 'a Var.t -> 'a t
val const : 'a -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t

(* ops *)
val add : int t -> int t -> int t
val sub : int t -> int t -> int t
val mul : int t -> int t -> int t
val div : int t -> int t -> int t
val mod_ : int t -> int t -> int t
val lshift : int t -> amt:int t -> int t
val rshift : int t -> amt:int t -> arith:bool -> int t
val bit_and : int t -> int t -> int t
val bit_or : int t -> int t -> int t
val bit_xor : int t -> int t -> int t
val eq : int t -> int t -> bool t
val ne : int t -> int t -> bool t
val not_ : bool t -> bool t

module Ir : sig
  type 'a outer = 'a t

  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : 'a -> 'a t
    | Map : Any.t t * (Any.t -> 'a) -> 'a t
    | Add : int t * int t -> int t
    | Sub : int t * int t -> int t
    | Mul : int t * int t -> int t
    | Div : int t * int t -> int t
    | Mod : int t * int t -> int t
    | LShift : int t * int t -> int t
    | LogicalRShift : int t * int t -> int t
    | ArithRShift : int t * int t -> int t
    | BitAnd : int t * int t -> int t
    | BitOr : int t * int t -> int t
    | BitXor : int t * int t -> int t
    | Eq : int t * int t -> bool t
    | Ne : int t * int t -> bool t
    | Not : bool t -> bool t
  [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t
  end

  val unwrap : 'a outer -> 'a t
  val untype : 'a t -> U.t
  val untype' : 'a outer -> U.t
end
