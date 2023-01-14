open! Core

module T = struct
  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : 'a * Width.t -> 'a t
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
    | Eq : Cint.t t * Cint.t t -> bool t
    | Ne : Cint.t t * Cint.t t -> bool t
    | Not : bool t -> bool t
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

let rec width : type a. a t -> Width.t = function
  | Var var_id -> Dtype.Ir.width var_id.u.d.dtype
  | Const (_, width) -> width
  | Map _ -> Unlimited
  | Add (a, b) -> Width.(max (width a) (width b) + one)
  | Sub (a, _) -> (* this is not allowed to underflow *) width a
  | Mul (a, b) -> Width.(width a + width b)
  | Div (a, _) -> width a
  | Mod (a, b) -> Width.min (width a) (width b)
  | LShift (_a, _b) -> Unlimited (* TODO *)
  | LogicalRShift (a, _) -> width a
  | BitAnd (a, b) -> Width.min (width a) (width b)
  | BitOr (a, b) -> Width.max (width a) (width b)
  | BitXor (a, b) -> Width.max (width a) (width b)
  | Eq (_, _) -> Width.one
  | Ne (_, _) -> Width.one
  | Not _ -> Width.one

module CBool_ = struct
  let var v = var v
  let true_ = Const (true, Fixed 1)
  let false_ = Const (false, Fixed 1)
  let not_ a = Not a
end

module String_ = struct
  let const s = Const (s, Unlimited)
end

module CInt_ = struct
  let var v = var v
  let const c = Const (c, Cint.width' c)
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
  let untype' = untype
end
