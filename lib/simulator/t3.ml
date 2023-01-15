open! Core
open! Act

type t = A | B | C [@@deriving sexp, equal]

let all = [ A; B; C ]
let to_int t = match t with A -> 0 | B -> 1 | C -> 2

(*
   let of_int i =
     match i with 0 -> A | 1 -> B | 2 -> C | _ -> failwith "invalid value of i"
*)
let bitwidth t = to_int t |> CInt.of_int |> CInt.bitwidth

let max_bitwidth =
  List.map all ~f:bitwidth
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let dtype =
  DType.create ~equal ~sexp_of_t
    ~max_layout_of:(fun t -> Bits_fixed (bitwidth t))
    ~layout:(Bits_fixed max_bitwidth)
