open! Core

type t [@@deriving sexp, equal]

val create_bitwidth : int -> t
val union : t -> t -> t

(* math operations *)
val eval_expr : 'a Expr.t -> of_var:('a -> t) -> t
val rewrite_expr : 'a Expr.t -> of_var:('a -> t) -> 'a Expr.t
