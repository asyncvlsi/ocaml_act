open! Core
open! Core
module Tag = Expr_tag

module Assert = struct
  type t = {
    guards : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t list;
    cond : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
    log_e : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
    f : Act_ir.CInt.t -> string;
  }
  [@@deriving sexp_of]
end

module K = struct
  type 'v t =
    | Var of 'v
    | Const of Act_ir.CInt.t
    | Add of 'v t * 'v t
    | Sub_no_wrap of 'v t * 'v t
    | Sub_wrap of 'v t * 'v t * int
    | Mul of 'v t * 'v t
    | Div of 'v t * 'v t
    | Mod of 'v t * 'v t
    | LShift of 'v t * 'v t
    | RShift of 'v t * 'v t
    | BitAnd of 'v t * 'v t
    | BitOr of 'v t * 'v t
    | BitXor of 'v t * 'v t
    | Eq of 'v t * 'v t
    | Ne of 'v t * 'v t
    | Lt of 'v t * 'v t
    | Le of 'v t * 'v t
    | Gt of 'v t * 'v t
    | Ge of 'v t * 'v t
    | Clip of 'v t * int
    (* This asserts that the first expression (which must have value 0 or 1) is
       1, and then returns the second value. In the simulator, if it is false,
       it calls the function for a nice error report. *)
    | With_assert_log of
        (* assert *)
        'v t
        * (* value *)
        'v t
        * (* log_input *)
        'v t
        * (Act_ir.CInt.t -> string)
  [@@deriving sexp_of]

  let unwrap e =
    let rec f assert_guards e =
      let ff e = f assert_guards e in
      match e with
      | Var v -> ([], Act_ir.Ir.Expr.Var v)
      | Const c -> ([], Const c)
      | Add (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Add (a, b))
      | Sub_no_wrap (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Sub_no_wrap (a, b))
      | Sub_wrap (a, b, bits) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Sub_wrap (a, b, bits))
      | Mul (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Mul (a, b))
      | Div (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Div (a, b))
      | Mod (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Mod (a, b))
      | LShift (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, LShift (a, b))
      | RShift (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, RShift (a, b))
      | BitAnd (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, BitAnd (a, b))
      | BitOr (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, BitOr (a, b))
      | BitXor (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, BitXor (a, b))
      | Eq (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Eq (a, b))
      | Ne (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Ne (a, b))
      | Lt (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Lt (a, b))
      | Le (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Le (a, b))
      | Gt (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Gt (a, b))
      | Ge (a, b) ->
          let a_asserts, a = ff a in
          let b_asserts, b = ff b in
          (a_asserts @ b_asserts, Ge (a, b))
      | Clip (a, bits) ->
          let a_asserts, a = ff a in
          (a_asserts, Clip (a, bits))
      | With_assert_log (cond, value, log_e, fn) ->
          let cond_asserts, cond = f assert_guards cond in
          let log_e_asserts, log_e =
            f
              (assert_guards
              @ [ Act_ir.Ir.Expr.Eq (Const Act_ir.CInt.zero, cond) ])
              log_e
          in
          let assert_ =
            { Assert.guards = assert_guards; cond; log_e; f = fn }
          in
          let value_asserts, value = f (assert_guards @ [ cond ]) value in
          (cond_asserts @ log_e_asserts @ [ assert_ ] @ value_asserts, value)
    in
    f [] e

  let wrap e =
    let rec f e =
      match e with
      | Act_ir.Ir.Expr.Var v -> Var v
      | Const c -> Const c
      | Add (a, b) -> Add (f a, f b)
      | Sub_no_wrap (a, b) -> Sub_no_wrap (f a, f b)
      | Sub_wrap (a, b, bits) -> Sub_wrap (f a, f b, bits)
      | Mul (a, b) -> Mul (f a, f b)
      | Div (a, b) -> Div (f a, f b)
      | Mod (a, b) -> Mod (f a, f b)
      | LShift (a, b) -> LShift (f a, f b)
      | RShift (a, b) -> RShift (f a, f b)
      | BitAnd (a, b) -> BitAnd (f a, f b)
      | BitOr (a, b) -> BitOr (f a, f b)
      | BitXor (a, b) -> BitXor (f a, f b)
      | Eq (a, b) -> Eq (f a, f b)
      | Ne (a, b) -> Ne (f a, f b)
      | Lt (a, b) -> Lt (f a, f b)
      | Le (a, b) -> Le (f a, f b)
      | Gt (a, b) -> Gt (f a, f b)
      | Ge (a, b) -> Ge (f a, f b)
      | Clip (a, bits) -> Clip (f a, bits)
    in
    f e
end

type 'a t = { k : Act_ir.Ir.Var.t K.t; tag : 'a Tag.t; max_bits : int }
[@@deriving sexp_of]

module U = struct
  type nonrec t = Act_ir.Utils.Any.t t [@@deriving sexp_of]
end

let var (v : 'a Var.t) =
  let dtype = Var.Internal.dtype v |> Ir_dtype.untype in
  let v = Var.Internal.unwrap v in
  let tag : 'a Expr_tag.t =
    let tag = Ir_dtype.expr_tag dtype in
    Obj.magic (tag : Act_ir.Utils.Any.t Expr_tag.t)
  in
  let max_bits = match Ir_dtype.layout dtype with Bits_fixed bits -> bits in
  { k = Var v; tag; max_bits }

let cint_tag = Expr_tag.cint_expr_tag
let cbool_tag = Expr_tag.cbool_expr_tag

let bool_of_int i =
  assert (Tag.equal cint_tag i.tag);
  let k = i.k in
  let k =
    K.With_assert_log
      ( Le (k, Const (Act_ir.CInt.of_int 1)),
        k,
        k,
        fun c -> [%string "Invalid cast Cint to Cbool: %{c#Act_ir.CInt}"] )
  in
  { k; tag = cbool_tag; max_bits = 1 }

let int_of_bool b =
  assert (Tag.equal cbool_tag b.tag);
  { k = b.k; tag = cint_tag; max_bits = 1 }

let of_cint c =
  { k = Const c; tag = cint_tag; max_bits = Act_ir.CInt.bitwidth c }

let of_int i = of_cint (Act_ir.CInt.of_int i)
let of_bool b = Cbool0.of_bool b |> Cbool0.to_cint |> of_cint |> bool_of_int
let zero = of_int 0
let one = of_int 1
let two = of_int 2
let three = of_int 3
let four = of_int 4
let five = of_int 5
let true_ = of_bool true
let false_ = of_bool false

let clip e ~bits =
  assert (Tag.equal cint_tag e.tag);
  let bits = Int.max bits 0 in
  { k = Clip (e.k, bits); tag = cint_tag; max_bits = bits }

let not_ b =
  assert (Tag.equal cbool_tag b.tag);
  { k = Eq (b.k, Const Act_ir.CInt.zero); tag = cbool_tag; max_bits = 1 }

let and_ a b =
  assert (Tag.equal cbool_tag a.tag);
  assert (Tag.equal cbool_tag b.tag);
  { k = BitAnd (a.k, b.k); tag = cbool_tag; max_bits = 1 }

let or_ a b =
  assert (Tag.equal cbool_tag a.tag);
  assert (Tag.equal cbool_tag b.tag);
  { k = BitOr (a.k, b.k); tag = cbool_tag; max_bits = 1 }

let bool_eq a b =
  assert (Tag.equal cbool_tag a.tag);
  assert (Tag.equal cbool_tag b.tag);
  { k = Eq (a.k, b.k); tag = cbool_tag; max_bits = 1 }

let bool_ne a b = bool_eq a b |> not_
let xor_ = bool_ne

(* ops *)
let add a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  {
    k = Add (a.k, b.k);
    tag = cint_tag;
    max_bits = 1 + Int.max a.max_bits b.max_bits;
  }

let sub a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Sub_no_wrap (a.k, b.k); tag = cint_tag; max_bits = a.max_bits }

let sub_wrap a b ~bits =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Sub_wrap (a.k, b.k, bits); tag = cint_tag; max_bits = bits }

let mul a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Mul (a.k, b.k); tag = cint_tag; max_bits = a.max_bits + b.max_bits }

let div a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Div (a.k, b.k); tag = cint_tag; max_bits = a.max_bits }

let mod_ a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  {
    k = Mod (a.k, b.k);
    tag = cint_tag;
    max_bits = Int.min a.max_bits b.max_bits;
  }

let left_shift a ~amt =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag amt.tag);
  {
    k = LShift (a.k, amt.k);
    tag = cint_tag;
    max_bits = a.max_bits + Int.pow 2 amt.max_bits - 1;
  }

let right_shift a ~amt =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag amt.tag);
  { k = RShift (a.k, amt.k); tag = cint_tag; max_bits = a.max_bits }

let left_shift' a ~amt =
  left_shift a ~amt:(of_int amt) |> clip ~bits:(a.max_bits + amt)

let right_shift' a ~amt =
  right_shift a ~amt:(of_int amt) |> clip ~bits:(a.max_bits - amt)

let bit_and a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  {
    k = BitAnd (a.k, b.k);
    tag = cint_tag;
    max_bits = Int.min a.max_bits b.max_bits;
  }

let bit_or a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  {
    k = BitOr (a.k, b.k);
    tag = cint_tag;
    max_bits = Int.max a.max_bits b.max_bits;
  }

let bit_xor a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  {
    k = BitXor (a.k, b.k);
    tag = cint_tag;
    max_bits = Int.max a.max_bits b.max_bits;
  }

let eq a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Eq (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let ne a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Ne (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let lt a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Lt (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let le a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Le (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let gt a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Gt (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let ge a b =
  assert (Tag.equal cint_tag a.tag);
  assert (Tag.equal cint_tag b.tag);
  { k = Ge (a.k, b.k); tag = cint_tag; max_bits = 1 } |> bool_of_int

let add_wrap a b ~bits = add a b |> clip ~bits

let with_assert_log ?new_max_bits ~assert_e ~val_e ~log_e log_fn =
  assert (Tag.equal cbool_tag assert_e.tag);
  let log_fn i =
    let v = Tag.value_of_cint log_e.tag i |> Option.value_exn in
    log_fn v
  in
  {
    k = With_assert_log (assert_e.k, val_e.k, log_e.k, log_fn);
    tag = val_e.tag;
    max_bits = Option.value new_max_bits ~default:val_e.max_bits;
  }

let var v = var v

module Internal = struct
  module Assert = Assert

  let unwrap t = K.unwrap t.k
  let tag t = t.tag
  let max_bits t = t.max_bits
  let wrap k tag max_bits = { k = K.wrap k; tag; max_bits }

  let with_set_tag_and_max_bits { k; tag = _; max_bits = old_max_bits } tag
      max_bits =
    { k; tag; max_bits = Int.min old_max_bits max_bits }
end
