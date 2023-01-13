open! Core

type 'a t [@@deriving sexp_of]

val var : 'a Var.t -> 'a t
val const : 'a -> 'a t
val cint : int -> Cint.t t
val map : 'a t -> f:('a -> 'b) -> 'b t

(* ops *)
val add : Cint.t t -> Cint.t t -> Cint.t t
val sub : Cint.t t -> Cint.t t -> Cint.t t
val mul : Cint.t t -> Cint.t t -> Cint.t t
val div : Cint.t t -> Cint.t t -> Cint.t t
val mod_ : Cint.t t -> Cint.t t -> Cint.t t
val lshift : Cint.t t -> amt:Cint.t t -> Cint.t t
val rshift : Cint.t t -> amt:Cint.t t -> arith:bool -> Cint.t t
val bit_and : Cint.t t -> Cint.t t -> Cint.t t
val bit_or : Cint.t t -> Cint.t t -> Cint.t t
val bit_xor : Cint.t t -> Cint.t t -> Cint.t t
val eq : Cint.t t -> Cint.t t -> bool t
val ne : Cint.t t -> Cint.t t -> bool t
val not_ : bool t -> bool t

module Ir : sig
  type 'a outer = 'a t

  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : 'a -> 'a t
    | Map : Any.t t * (Any.t -> 'a) -> 'a t
    | Add : Cint.t t * Cint.t t -> Cint.t t
    | Sub : Cint.t t * Cint.t t -> Cint.t t
    | Mul : Cint.t t * Cint.t t -> Cint.t t
    | Div : Cint.t t * Cint.t t -> Cint.t t
    | Mod : Cint.t t * Cint.t t -> Cint.t t
    | LShift : Cint.t t * Cint.t t -> Cint.t t
    | LogicalRShift : Cint.t t * Cint.t t -> Cint.t t
    | ArithRShift : Cint.t t * Cint.t t -> Cint.t t
    | BitAnd : Cint.t t * Cint.t t -> Cint.t t
    | BitOr : Cint.t t * Cint.t t -> Cint.t t
    | BitXor : Cint.t t * Cint.t t -> Cint.t t
    | Eq : Cint.t t * Cint.t t -> bool t
    | Ne : Cint.t t * Cint.t t -> bool t
    | Not : bool t -> bool t
  [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t
  end

  val unwrap : 'a outer -> 'a t
  val untype : 'a t -> U.t
  val untype' : 'a outer -> U.t
end
