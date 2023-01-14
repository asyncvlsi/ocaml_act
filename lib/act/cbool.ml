open! Core
include Cint

let true_ = of_int 1
let false_ = of_int 0
let of_bool b = if b then true_ else false_
