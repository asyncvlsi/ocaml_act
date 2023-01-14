open! Core

type 'a t [@@deriving sexp_of]

val var : 'a Var.t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t

module CBool_ : sig
  val var : Cbool.t Var.t -> Cbool.t t
  val true_ : Cbool.t t
  val false_ : Cbool.t t
  val not_ : Cbool.t t -> Cbool.t t
  (* val and_ : Cbool.t t -> Cbool.t t -> Cbool.t t
     val or_ : Cbool.t t -> Cbool.t t -> Cbool.t t
     val xor_ : Cbool.t t -> Cbool.t t -> Cbool.t t *)
end

module String_ : sig
  val const : string -> string t
end

module CInt_ : sig
  val var : Cint.t Var.t -> Cint.t t
  val const : Cint.t -> Cint.t t
  val cint : int -> Cint.t t

  (* ops *)
  val add : Cint.t t -> Cint.t t -> Cint.t t
  val sub : Cint.t t -> Cint.t t -> Cint.t t
  val mul : Cint.t t -> Cint.t t -> Cint.t t
  val div : Cint.t t -> Cint.t t -> Cint.t t
  val mod_ : Cint.t t -> Cint.t t -> Cint.t t
  val lshift : Cint.t t -> amt:Cint.t t -> Cint.t t
  val rshift : Cint.t t -> amt:Cint.t t -> Cint.t t
  val bit_and : Cint.t t -> Cint.t t -> Cint.t t
  val bit_or : Cint.t t -> Cint.t t -> Cint.t t
  val bit_xor : Cint.t t -> Cint.t t -> Cint.t t
  val eq : Cint.t t -> Cint.t t -> Cbool.t t
  val ne : Cint.t t -> Cint.t t -> Cbool.t t
end

module Ir : sig
  type 'a outer = 'a t

  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : 'a * Layout.t -> 'a t
    | Map : Any.t t * (Any.t -> 'a) -> 'a t
    | Add : Cint.t t * Cint.t t -> Cint.t t
    | Sub : Cint.t t * Cint.t t -> Cint.t t
    | Mul : Cint.t t * Cint.t t -> Cint.t t
    | Div : Cint.t t * Cint.t t -> Cint.t t
    | Mod : Cint.t t * Cint.t t -> Cint.t t
    | LShift : Cint.t t * Cint.t t -> Cint.t t
    | LogicalRShift : Cint.t t * Cint.t t -> Cint.t t
    | BitAnd : Cint.t t * Cint.t t -> Cint.t t
    | BitOr : Cint.t t * Cint.t t -> Cint.t t
    | BitXor : Cint.t t * Cint.t t -> Cint.t t
    | Eq : Cint.t t * Cint.t t -> Cbool.t t
    | Ne : Cint.t t * Cint.t t -> Cbool.t t
    | Not : Cbool.t t -> Cbool.t t
  [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t
  end

  val max_layout : 'a t -> Layout.t
  val unwrap : 'a outer -> 'a t
  val untype : 'a t -> U.t
  val untype' : 'a outer -> U.t
end
