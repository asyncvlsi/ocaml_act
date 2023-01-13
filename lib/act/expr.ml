open! Core

module T = struct
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

  (* internal *)
  let untype t : Any.t t = Obj.magic t

  module U = struct
    type nonrec t = Any.t t
  end

  (* main operations *)
  let var v = Var (Var.Ir.unwrap v)
  let const c = Const c
  let cint i = Const (Cint.of_int i)
  let map (e : 'a t) ~(f : 'a -> 'b) = Map (untype e, Obj.magic f)

  (* ops *)
  let add a b = Add (a, b)
  let sub a b = Sub (a, b)
  let mul a b = Mul (a, b)
  let div a b = Div (a, b)
  let mod_ a b = Mod (a, b)
  let lshift a ~amt = LShift (a, amt)

  let rshift a ~amt ~arith =
    if arith then ArithRShift (a, amt) else LogicalRShift (a, amt)

  let bit_and a b = BitAnd (a, b)
  let bit_or a b = BitOr (a, b)
  let bit_xor a b = BitXor (a, b)
  let eq a b = Eq (a, b)
  let ne a b = Ne (a, b)
  let not_ a = Not a
end

include T

module Ir = struct
  include T

  type 'a outer = 'a t

  let unwrap t = t
  let untype' = untype
end
