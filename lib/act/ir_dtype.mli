open! Core

type 'a t = {
  equal : 'a -> 'a -> bool;
  sexp_of_t : 'a -> Sexp.t;
  max_layout_of : 'a -> Layout.t;
  cint_of : 'a -> Act_ir.CInt.t;
  of_cint : Act_ir.CInt.t -> 'a option;
  sexp_of_cint : Act_ir.CInt.t -> Sexp.t;
  of_cint_assert_expr_fn : unit Act_ir.Expr.t;
  layout : Layout.t;
  expr_tag : 'a Expr_tag.t;
}
[@@deriving sexp_of]

val create :
  equal:('a -> 'a -> bool) ->
  sexp_of_t:('a -> Sexp.t) ->
  max_layout_of:('a -> Layout.t) ->
  cint_of:('a -> Act_ir.CInt.t) ->
  of_cint:(Act_ir.CInt.t -> 'a option) ->
  of_cint_assert_expr_fn:unit Act_ir.Expr.t ->
  layout:Layout.t ->
  expr_tag:'a Expr_tag.t ->
  'a t

val equal_ : 'a t -> 'a -> 'a -> bool
val sexp_of_t_ : 'a t -> 'a -> Sexp.t
val cint_of_value : 'a t -> 'a -> Act_ir.CInt.t
val of_cint_assert_expr_fn : 'a t -> unit Act_ir.Expr.t
val value_of_cint_exn : 'a t -> Act_ir.CInt.t -> 'a
val equal_fn : 'a t -> ('a -> 'a -> bool) Staged.t
val sexp_of_t_fn : 'a t -> ('a -> Sexp.t) Staged.t
val max_layout_of : 'a t -> 'a -> Layout.t
val layout : 'a t -> Layout.t
val fits_into_dtype : into:'a t -> Layout.t -> bool
val fits_value : 'a t -> 'a -> bool
val expr_tag : 'a t -> 'a Expr_tag.t
val untype : 'a t -> Act_ir.Utils.Any.t t

(* premade dtypes *)

val dummy_val : Act_ir.Utils.Any.t t
val cbool_dtype : Act_ir.CBool.t t
val cint_dtype : bits:int -> Act_ir.CInt.t t
