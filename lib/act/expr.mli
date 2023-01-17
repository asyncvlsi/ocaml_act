open! Core

module Wrap : sig
  type 'a t [@@deriving sexp_of]

  val var : 'a Var.Wrap.t -> 'a t
end

module CInt_ : sig
  val var : Cint0.t Var.Wrap.t -> Cint0.t Wrap.t
  val const : Cint0.t -> Cint0.t Wrap.t
  val cint : int -> Cint0.t Wrap.t

  (* ops *)
  val add : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val sub : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val mul : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val div : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val mod_ : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val lshift : Cint0.t Wrap.t -> amt:Cint0.t Wrap.t -> Cint0.t Wrap.t
  val rshift : Cint0.t Wrap.t -> amt:Cint0.t Wrap.t -> Cint0.t Wrap.t
  val bit_and : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val bit_or : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val bit_xor : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cint0.t Wrap.t
  val eq : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cbool0.t Wrap.t
  val ne : Cint0.t Wrap.t -> Cint0.t Wrap.t -> Cbool0.t Wrap.t
  val clip : Cint0.t Wrap.t -> bits:int -> Cint0.t Wrap.t
  val add_wrap : Cint0.t Wrap.t -> Cint0.t Wrap.t -> bits:int -> Cint0.t Wrap.t
  val sub_wrap : Cint0.t Wrap.t -> Cint0.t Wrap.t -> bits:int -> Cint0.t Wrap.t
end

module Ir : sig
  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : Cint0.t -> Cint0.t t
    | Add : Cint0.t t * Cint0.t t -> Cint0.t t
    | Sub : Cint0.t t * Cint0.t t -> Cint0.t t
    | Mul : Cint0.t t * Cint0.t t -> Cint0.t t
    | Div : Cint0.t t * Cint0.t t -> Cint0.t t
    | Mod : Cint0.t t * Cint0.t t -> Cint0.t t
    | LShift : Cint0.t t * Cint0.t t -> Cint0.t t
    | LogicalRShift : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitAnd : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitOr : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitXor : Cint0.t t * Cint0.t t -> Cint0.t t
    | Eq : Cint0.t t * Cint0.t t -> Cbool0.t t
    | Ne : Cint0.t t * Cint0.t t -> Cbool0.t t
    | Not : Cbool0.t t -> Cbool0.t t
    | Clip : Cint0.t t * int -> Cint0.t t
    | Add_wrap : Cint0.t t * Cint0.t t * int -> Cint0.t t
    | Sub_wrap : Cint0.t t * Cint0.t t * int -> Cint0.t t
    | Magic_EnumToCInt : Any.t t * (Any.t -> Cint0.t) -> Cint0.t t
    | Magic_EnumOfCInt : Cint0.t t * (Cint0.t -> 'a option) -> 'a t
  [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t
  end

  val max_layout : 'a t -> Layout.t
  val unwrap : 'a Wrap.t -> 'a t
  val magic_EnumToCInt : 'a Wrap.t -> f:('a -> Cint0.t) -> Cint0.t Wrap.t
  val magic_EnumOfCInt : Cint0.t Wrap.t -> f:(Cint0.t -> 'a option) -> 'a Wrap.t
  val wrap : 'a t -> 'a Wrap.t
  val untype : 'a t -> U.t
  val untype' : 'a Wrap.t -> U.t
end
