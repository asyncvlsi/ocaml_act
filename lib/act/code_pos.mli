open! Core

type t = {
  filename : string;
  line_number : int;
  start_char : int;
  end_char : int;
}
[@@deriving sexp]

val dummy_loc : t

(* The location the the function which called this function was called from.
   i.e. the "parent's call site" *)
val psite : ?depth:int -> unit -> t
val value_or_psite : ?depth:int -> t option -> t
