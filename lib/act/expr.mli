open! Core

type 'a t [@@deriving sexp_of]

val var : 'a Var.t -> 'a t
val bool_of_int : Cint0.t t -> Cbool0.t t
val int_of_bool : Cbool0.t t -> Cint0.t t

val with_assert_log :
  ?new_max_bits:int ->
  assert_e:Cbool0.t t ->
  val_e:'a t ->
  log_e:'b t ->
  ('b -> string) ->
  'a t

val of_bool : bool -> Cbool0.t t
val of_int : int -> Cint0.t t
val of_cint : Cint0.t -> Cint0.t t

(* ops *)
val add : Cint0.t t -> Cint0.t t -> Cint0.t t
val sub : Cint0.t t -> Cint0.t t -> Cint0.t t
val mul : Cint0.t t -> Cint0.t t -> Cint0.t t
val div : Cint0.t t -> Cint0.t t -> Cint0.t t
val mod_ : Cint0.t t -> Cint0.t t -> Cint0.t t
val left_shift : Cint0.t t -> amt:Cint0.t t -> Cint0.t t
val right_shift : Cint0.t t -> amt:Cint0.t t -> Cint0.t t
val left_shift' : Cint0.t t -> amt:int -> Cint0.t t
val right_shift' : Cint0.t t -> amt:int -> Cint0.t t
val bit_and : Cint0.t t -> Cint0.t t -> Cint0.t t
val bit_or : Cint0.t t -> Cint0.t t -> Cint0.t t
val bit_xor : Cint0.t t -> Cint0.t t -> Cint0.t t
val eq : Cint0.t t -> Cint0.t t -> Cbool0.t t
val ne : Cint0.t t -> Cint0.t t -> Cbool0.t t
val lt : Cint0.t t -> Cint0.t t -> Cbool0.t t
val le : Cint0.t t -> Cint0.t t -> Cbool0.t t
val gt : Cint0.t t -> Cint0.t t -> Cbool0.t t
val ge : Cint0.t t -> Cint0.t t -> Cbool0.t t
val clip : Cint0.t t -> bits:int -> Cint0.t t
val add_wrap : Cint0.t t -> Cint0.t t -> bits:int -> Cint0.t t
val sub_wrap : Cint0.t t -> Cint0.t t -> bits:int -> Cint0.t t
val not_ : Cbool0.t t -> Cbool0.t t
val and_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val or_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val xor_ : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val bool_eq : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val bool_ne : Cbool0.t t -> Cbool0.t t -> Cbool0.t t
val zero : Cint0.t t
val one : Cint0.t t
val two : Cint0.t t
val three : Cint0.t t
val four : Cint0.t t
val five : Cint0.t t
val true_ : Cbool0.t t
val false_ : Cbool0.t t

module Ir : sig
  module Tag = Expr_tag

  val cbool_tag : Cbool0.t Expr_tag.t
  val cint_tag : Cint0.t Expr_tag.t

  module K = Expr0

  type 'a outer = 'a t

  type 'a t = { k : Var.Ir.U.t K.t; tag : 'a Tag.t; max_bits : int }
  [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t [@@deriving sexp_of]
  end

  val max_layout : 'a t -> Layout.t
  val unwrap : 'a outer -> 'a t
  val wrap : 'a t -> 'a outer
  val untype : 'a t -> U.t
  val untype' : 'a outer -> U.t
end
