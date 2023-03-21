open! Core
include Bool

let of_bool b = b
let to_bool b = b
let bitwidth _ = 1
let to_cint t = match t with true -> Cint0.of_int 1 | false -> Cint0.of_int 0

let of_cint i =
  if Cint0.(equal i (of_int 1)) then Some true
  else if Cint0.(equal i (of_int 0)) then Some false
  else None

let not_ b = not b
let and_ a b = a && b
let or_ a b = a || b
let bool_eq a b = Bool.equal a b
let bool_neq a b = not (Bool.equal a b)
let xor_ a b = bool_neq a b
