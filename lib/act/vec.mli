open! Core

type 'a t [@@deriving sexp]

val to_array : 'a t -> 'a array
val create : cap:int -> default:'a -> 'a t
val of_array : 'a array -> default:'a -> 'a t
val at : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val remove : 'a t -> int -> unit
val push : 'a t -> 'a -> unit
val extend : 'a t -> 'a list -> unit
val is_empty : 'a t -> bool
val length : 'a t -> int
val find : 'a t -> f:('a -> bool) -> 'a option
val iter : 'a t -> f:('a -> unit) -> unit
val clear : 'a t -> unit
