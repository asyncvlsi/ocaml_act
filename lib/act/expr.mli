open! Core

module Wrap : sig
  type 'a t [@@deriving sexp_of]

  val var : 'a Var.Wrap.t -> 'a t
end

module Ir : sig
  module Tag = Expr_tag

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

  type 'a t = { k : K.t; tag : 'a Tag.t; max_bits : int } [@@deriving sexp_of]

  module U : sig
    type nonrec t = Any.t t [@@deriving sexp_of]
  end

  val max_layout : 'a t -> Layout.t
  val unwrap : 'a Wrap.t -> 'a t
  val wrap : 'a t -> 'a Wrap.t
  val untype : 'a t -> U.t
  val untype' : 'a Wrap.t -> U.t
end

module CBool : sig
  val tag : Cbool0.t Expr_tag.t
  val of_int : Cint0.t Wrap.t -> Cbool0.t Wrap.t
  val to_int : Cbool0.t Wrap.t -> Cint0.t Wrap.t
end

val with_assert_log :
  ?new_max_bits:int ->
  assert_e:Cbool0.t Wrap.t ->
  val_e:'a Wrap.t ->
  log_e:'b Wrap.t ->
  ('b -> string) ->
  'a Wrap.t

module CInt : sig
  type nonrec t = Cint0.t Wrap.t [@@deriving sexp_of]

  val var : Cint0.t Var.Wrap.t -> t
  val const : Cint0.t -> t
  val cint : int -> t
  val tag : Cint0.t Expr_tag.t

  (* ops *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val mod_ : t -> t -> t
  val lshift : t -> amt:t -> t
  val rshift : t -> amt:t -> t
  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val eq : t -> t -> Cbool0.t Wrap.t
  val ne : t -> t -> Cbool0.t Wrap.t
  val lt : t -> t -> Cbool0.t Wrap.t
  val le : t -> t -> Cbool0.t Wrap.t
  val gt : t -> t -> Cbool0.t Wrap.t
  val ge : t -> t -> Cbool0.t Wrap.t
  val clip : t -> bits:int -> t
  val add_wrap : t -> t -> bits:int -> t
  val sub_wrap : t -> t -> bits:int -> t
end
