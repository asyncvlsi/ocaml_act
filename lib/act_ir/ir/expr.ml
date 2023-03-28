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
  | Eq0 of 'v t
  | Eq of 'v t * 'v t
  | Ne of 'v t * 'v t
  | Lt of 'v t * 'v t
  | Le of 'v t * 'v t
  | Gt of 'v t * 'v t
  | Ge of 'v t * 'v t
  | Clip of 'v t * int
  | Concat of ('v t * int) list
  | Log2OneHot of 'v t
[@@deriving sexp_of, compare, equal, hash]

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

(* utils *)

let bind_vars e ~f =
  let rec h e =
    match e with
    | Var v -> f v
    | Const c -> Const c
    | Add (a, b) -> Add (h a, h b)
    | Sub_no_wrap (a, b) -> Sub_no_wrap (h a, h b)
    | Sub_wrap (a, b, bits) -> Sub_wrap (h a, h b, bits)
    | Mul (a, b) -> Mul (h a, h b)
    | Div (a, b) -> Div (h a, h b)
    | Mod (a, b) -> Mod (h a, h b)
    | Eq (a, b) -> Eq (h a, h b)
    | Ne (a, b) -> Ne (h a, h b)
    | Gt (a, b) -> Gt (h a, h b)
    | Ge (a, b) -> Ge (h a, h b)
    | Lt (a, b) -> Lt (h a, h b)
    | Le (a, b) -> Le (h a, h b)
    | Eq0 a -> Eq0 (h a)
    | BitXor (a, b) -> BitXor (h a, h b)
    | BitOr (a, b) -> BitOr (h a, h b)
    | BitAnd (a, b) -> BitAnd (h a, h b)
    | LShift (a, b) -> LShift (h a, h b)
    | RShift (a, b) -> RShift (h a, h b)
    | Clip (a, bits) -> Clip (h a, bits)
    | Concat es -> Concat (List.map es ~f:(fun (e, bits) -> (h e, bits)))
    | Log2OneHot e -> Log2OneHot (h e)
  in
  h e

let map_vars e ~f = bind_vars e ~f:(fun v -> Var (f v))

let var_ids e =
  let rec f e =
    match e with
    | Var v -> [ v ]
    | Const _ -> []
    | Add (a, b) -> f a @ f b
    | Sub_no_wrap (a, b) -> f a @ f b
    | Sub_wrap (a, b, _) -> f a @ f b
    | Mul (a, b) -> f a @ f b
    | Div (a, b) -> f a @ f b
    | Mod (a, b) -> f a @ f b
    | Eq (a, b) -> f a @ f b
    | Ne (a, b) -> f a @ f b
    | Gt (a, b) -> f a @ f b
    | Ge (a, b) -> f a @ f b
    | Lt (a, b) -> f a @ f b
    | Le (a, b) -> f a @ f b
    | Eq0 a -> f a
    | BitXor (a, b) -> f a @ f b
    | BitOr (a, b) -> f a @ f b
    | BitAnd (a, b) -> f a @ f b
    | LShift (a, b) -> f a @ f b
    | RShift (a, b) -> f a @ f b
    | Clip (a, _) -> f a
    | Concat es -> List.concat_map es ~f:(fun (e, _) -> f e)
    | Log2OneHot e -> f e
  in
  f e

let bitwidth e ~bits_of_var =
  let rec h e =
    match e with
    | Var v -> bits_of_var v
    | Const c -> CInt.bitwidth c
    | Add (a, b) -> 1 + Int.max (h a) (h b)
    | Sub_no_wrap (a, _) -> h a
    | Sub_wrap (_, _, bits) -> bits
    | Mul (a, b) -> h a + h b
    | Div (a, _) -> h a
    | Mod (a, b) -> Int.min (h a) (h b)
    | Eq (_, _) -> 1
    | Ne (_, _) -> 1
    | Gt (_, _) -> 1
    | Ge (_, _) -> 1
    | Lt (_, _) -> 1
    | Le (_, _) -> 1
    | Eq0 _ -> 1
    | BitXor (a, b) -> Int.max (h a) (h b)
    | BitOr (a, b) -> Int.max (h a) (h b)
    | BitAnd (a, b) -> Int.min (h a) (h b)
    | LShift (a, b) -> h a + Int.pow 2 (h b) - 1
    | RShift (a, _) -> h a
    | Clip (a, bits) -> Int.min (h a) bits
    | Concat es -> List.sum (module Int) es ~f:snd
    | Log2OneHot e -> Int.ceil_log2 (h e)
  in
  h e
