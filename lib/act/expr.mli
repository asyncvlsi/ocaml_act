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

  module K : sig
    type t =
      | Var of Var.Ir.U.t
      | Const of Cint0.t
      | Add of t * t
      | Sub_no_wrap of t * t
      | Sub_wrap of t * t * int
      | Mul of t * t
      | Div of t * t
      | Mod of t * t
      | LShift of t * t
      | LogicalRShift of t * t
      | BitAnd of t * t
      | BitOr of t * t
      | BitXor of t * t
      | Eq of t * t
      | Ne of t * t
      | Lt of t * t
      | Le of t * t
      | Gt of t * t
      | Ge of t * t
      | Clip of t * int
      (* This asserts that the first expression (which must have value 0 or 1) is 1, and then returns the second value.
         In the simulator, if it is false, it calls the function for a nice error report. *)
      | With_assert_log of
          (* assert *) t
          * (* value *) t
          * (* log_input *) t
          * (Cint0.t -> string)
        (* It is undefined behavior for a act program to return Some from this function. In practice,
           this is only checked in the simulator, and not used in simulation. If you want it used for
           optimization, use With_assert or With_assert_log instead *)
    [@@deriving sexp_of]
  end

  type 'a outer = 'a t
  type 'a t = { k : K.t; tag : 'a Tag.t; max_bits : int } [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t [@@deriving sexp_of]
  end

  val max_layout : 'a t -> Layout.t
  val unwrap : 'a outer -> 'a t
  val wrap : 'a t -> 'a outer
  val untype : 'a t -> U.t
  val untype' : 'a outer -> U.t
end
