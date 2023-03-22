open! Core

type u
type t = (u[@sexp.opaque]) [@@deriving sexp_of]

let of_magic = Obj.magic
let of_int = of_magic
let of_string = of_magic
let of_bool = of_magic
let to_magic = Obj.magic
let to_int = to_magic
let to_string = to_magic
let to_bool = to_magic

module Array = struct
  let of_magic = Obj.magic
  let to_magic = Obj.magic
end

module Option = struct
  let of_magic = Obj.magic
  let to_magic = Obj.magic
end
