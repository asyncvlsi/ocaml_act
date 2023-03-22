open! Core

type 'a t [@@deriving sexp_of]

val var : 'a Var.t -> 'a t
val bool_of_int : Act_ir.CInt.t t -> Cbool0.t t
val int_of_bool : Cbool0.t t -> Act_ir.CInt.t t

val with_assert_log :
  ?new_max_bits:int ->
  assert_e:Cbool0.t t ->
  val_e:'a t ->
  log_e:'b t ->
  ('b -> string) ->
  'a t

val of_bool : bool -> Cbool0.t t
val of_int : int -> Act_ir.CInt.t t
val of_cint : Act_ir.CInt.t -> Act_ir.CInt.t t

(* ops *)
val add : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val sub : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val mul : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val div : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val mod_ : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val left_shift : Act_ir.CInt.t t -> amt:Act_ir.CInt.t t -> Act_ir.CInt.t t
val right_shift : Act_ir.CInt.t t -> amt:Act_ir.CInt.t t -> Act_ir.CInt.t t
val left_shift' : Act_ir.CInt.t t -> amt:int -> Act_ir.CInt.t t
val right_shift' : Act_ir.CInt.t t -> amt:int -> Act_ir.CInt.t t
val bit_and : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val bit_or : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val bit_xor : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Act_ir.CInt.t t
val eq : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val ne : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val lt : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val le : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val gt : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val ge : Act_ir.CInt.t t -> Act_ir.CInt.t t -> Cbool0.t t
val clip : Act_ir.CInt.t t -> bits:int -> Act_ir.CInt.t t
val add_wrap : Act_ir.CInt.t t -> Act_ir.CInt.t t -> bits:int -> Act_ir.CInt.t t
val sub_wrap : Act_ir.CInt.t t -> Act_ir.CInt.t t -> bits:int -> Act_ir.CInt.t t
val not_ : Cbool0.t t -> Cbool0.t t
val and_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val or_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val xor_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val bool_eq : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val bool_ne : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val zero : Act_ir.CInt.t t
val one : Act_ir.CInt.t t
val two : Act_ir.CInt.t t
val three : Act_ir.CInt.t t
val four : Act_ir.CInt.t t
val five : Act_ir.CInt.t t
val true_ : Cbool0.t t
val false_ : Cbool0.t t

(**/**)

module Internal : sig
  module Assert : sig
    type t = {
      guards : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t list;
      cond : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
      log_e : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
      f : Act_ir.CInt.t -> string;
    }
    [@@deriving sexp_of]
  end

  val unwrap : 'a t -> Assert.t list * Act_ir.Ir.Var.t Act_ir.Ir.Expr.t
  val max_bits : 'a t -> int
  val tag : 'a t -> 'a Expr_tag.t
  val wrap : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t -> 'a Expr_tag.t -> int -> 'a t
  val with_set_tag_and_max_bits : 'a t -> 'b Expr_tag.t -> int -> 'b t
end

(**/**)
