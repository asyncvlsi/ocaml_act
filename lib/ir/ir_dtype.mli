open! Core

type 'a t = {
  equal : 'a -> 'a -> bool;
  sexp_of_t : 'a -> Sexp.t;
  max_layout_of : 'a -> Ir_layout.t;
  cint_of : 'a -> Cint0.t;
  of_cint : Cint0.t -> 'a option;
  of_cint_assert_expr_fn : unit Ir_expr0.t;
  layout : Ir_layout.t;
  expr_tag : 'a Ir_expr_tag.t;
}
[@@deriving sexp_of]

val create :
  equal:('a -> 'a -> bool) ->
  sexp_of_t:('a -> Sexp.t) ->
  max_layout_of:('a -> Ir_layout.t) ->
  cint_of:('a -> Cint0.t) ->
  of_cint:(Cint0.t -> 'a option) ->
  of_cint_assert_expr_fn:unit Ir_expr0.t ->
  layout:Ir_layout.t ->
  expr_tag:'a Ir_expr_tag.t ->
  'a t

val equal_ : 'a t -> 'a -> 'a -> bool
val sexp_of_t_ : 'a t -> 'a -> Sexp.t
val cint_of_value : 'a t -> 'a -> Cint0.t
val of_cint_assert_expr_fn : 'a t -> unit Ir_expr0.t
val value_of_cint_exn : 'a t -> Cint0.t -> 'a
val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
val max_layout_of : 'a t -> 'a -> Ir_layout.t
val layout : 'a t -> Ir_layout.t
val fits_into_dtype : into:'a t -> Ir_layout.t -> bool
val fits_value : 'a t -> 'a -> bool
val expr_tag : 'a t -> 'a Ir_expr_tag.t
val untype : 'a t -> Any.t t

(* premade dtypes *)

val dummy_val : Any.t t
val cbool_dtype : Cbool0.t t
val cint_dtype : bits:int -> Cint0.t t
