open! Core

type 'a t [@@deriving sexp, compare, equal, hash]

val of_list_exn : 'a list -> 'a t
val of_list_opt : 'a list -> 'a t option
val opt_to_list : 'a t option -> 'a list
val to_list : 'a t -> 'a list
val concat : 'a t list -> 'a t option
val singleton : 'a -> 'a t
