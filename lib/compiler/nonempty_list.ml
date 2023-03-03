open! Core

type 'a t = 'a list [@@deriving sexp, compare, equal, hash]

let of_list_opt l = match l with [] -> None | l -> Some l
let of_list_exn l = of_list_opt l |> Option.value_exn
let opt_to_list l = match l with None -> [] | Some l -> l
let to_list l = l
let concat l = List.concat l |> of_list_opt
let singleton a = [ a ]
