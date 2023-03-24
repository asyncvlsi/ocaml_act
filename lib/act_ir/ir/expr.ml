open! Core
open Utils

type 'v t =
  | Var of 'v
  | Const of CInt.t
  | Add of 'v t * 'v t
  | Sub_no_wrap of 'v t * 'v t
  | Sub_wrap of 'v t * 'v t * int
  | Mul of 'v t * 'v t
  | Div of 'v t * 'v t
  | Mod of 'v t * 'v t
  | LShift of 'v t * 'v t
  | RShift of 'v t * 'v t
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
[@@deriving sexp_of]

let of_int i = Const (CInt.of_int i)
let of_cint i = Const i
let zero = of_int 0
let one = of_int 1
let two = of_int 2
let three = of_int 3
let four = of_int 4
let five = of_int 5
let true_ = of_int 1
let false_ = of_int 0
let var v = Var v
let add a b = Add (a, b)
let sub a b = Sub_no_wrap (a, b)
let mul a b = Mul (a, b)
let div a b = Div (a, b)
let mod_ a b = Mod (a, b)
let left_shift t ~amt = LShift (t, amt)
let right_shift t ~amt = RShift (t, amt)
let left_shift' t ~amt = LShift (t, of_int amt)
let right_shift' t ~amt = RShift (t, of_int amt)
let bit_and a b = BitAnd (a, b)
let bit_or a b = BitOr (a, b)
let bit_xor a b = BitXor (a, b)
let eq a b = Eq (a, b)
let ne a b = Ne (a, b)
let lt a b = Lt (a, b)
let le a b = Le (a, b)
let gt a b = Gt (a, b)
let ge a b = Ge (a, b)
let clip t ~bits = Clip (t, bits)
let not_ t = eq t false_
let and_ a b = BitAnd (a, b)
let or_ a b = BitOr (a, b)
let xor_ a b = BitXor (a, b)
