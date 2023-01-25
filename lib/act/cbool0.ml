open! Core
include Bool

let of_bool b = b
let bitwidth _ = 1
let to_cint t = match t with true -> Cint0.of_int 1 | false -> Cint0.of_int 0

let of_cint i =
  if Cint0.(equal i (of_int 1)) then Some true
  else if Cint0.(equal i (of_int 0)) then Some false
  else None

let expr_tag = Expr_tag.create ~cint_of_value:to_cint ~value_of_cint:of_cint
