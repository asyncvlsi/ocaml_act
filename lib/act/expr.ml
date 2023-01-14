open! Core

module T = struct
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
end

include T

(* internal *)
let untype t : Any.t t = Obj.magic t

module U = struct
  type nonrec t = Any.t t
end

(* main operations *)
let var v = Var (Var.Ir.unwrap v)
let map (e : 'a t) ~(f : 'a -> 'b) = Map (untype e, Obj.magic f)

module Layout = struct
  include Layout

  let imax a b =
    match (a, b) with
    | Fixed a, Fixed b -> Fixed (Int.max a b)
    | Unknown, Fixed _ | Fixed _, Unknown | Unknown, Unknown -> Unknown

  let imin a b =
    match (a, b) with
    | Fixed a, Fixed b -> Fixed (Int.min a b)
    | Unknown, Fixed a | Fixed a, Unknown -> Fixed a
    | Unknown, Unknown -> Unknown

  let iadd a b =
    match (a, b) with
    | Fixed a, Fixed b -> Fixed (a + b)
    | Unknown, Fixed _ | Fixed _, Unknown | Unknown, Unknown -> Unknown

  let iadd1 a = match a with Fixed a -> Fixed (a + 1) | Unknown -> Unknown

  let ilshift a amt =
    match (a, amt) with
    | Fixed a, Fixed amt -> Fixed (a + Int.pow 2 amt - 1)
    | Unknown, Fixed _ | Fixed _, Unknown | Unknown, Unknown -> Unknown
end

let rec max_layout : type a. a t -> Layout.t = function
  | Var var_id -> Dtype.Ir.layout var_id.u.d.dtype
  | Const (_, layout) -> layout
  | Map _ -> Unknown
  | Add (a, b) -> Layout.(iadd1 (imax (max_layout a) (max_layout b)))
  | Sub (a, _) -> (* this is not allowed to underflow *) max_layout a
  | Mul (a, b) -> Layout.(iadd (max_layout a) (max_layout b))
  | Div (a, _) -> max_layout a
  | Mod (a, b) -> Layout.imin (max_layout a) (max_layout b)
  | LShift (a, b) -> Layout.ilshift (max_layout a) (max_layout b)
  | LogicalRShift (a, _) -> max_layout a
  | BitAnd (a, b) -> Layout.imin (max_layout a) (max_layout b)
  | BitOr (a, b) -> Layout.imax (max_layout a) (max_layout b)
  | BitXor (a, b) -> Layout.imax (max_layout a) (max_layout b)
  | Eq (_, _) -> Fixed 1
  | Ne (_, _) -> Fixed 1
  | Not _ -> Fixed 1

module CBool_ = struct
  let var v = var v
  let true_ = Const (Cbool.true_, Fixed 1)
  let false_ = Const (Cbool.false_, Fixed 1)
  let not_ a = Not a
end

module String_ = struct
  let const s = Const (s, Unknown)
end

module CInt_ = struct
  let var v = var v
  let const c = Const (c, Fixed (Cint.bitwidth c))
  let cint i = const (Cint.of_int i)

  (* ops *)
  let add a b = Add (a, b)
  let sub a b = Sub (a, b)
  let mul a b = Mul (a, b)
  let div a b = Div (a, b)
  let mod_ a b = Mod (a, b)
  let lshift a ~amt = LShift (a, amt)
  let rshift a ~amt = LogicalRShift (a, amt)
  let bit_and a b = BitAnd (a, b)
  let bit_or a b = BitOr (a, b)
  let bit_xor a b = BitXor (a, b)
  let eq a b = Eq (a, b)
  let ne a b = Ne (a, b)
end

module Ir = struct
  include T
  module U = U

  type 'a outer = 'a t

  let untype t = untype t
  let unwrap t = t
  let untype' t = untype t
  let max_layout t = max_layout t
end
