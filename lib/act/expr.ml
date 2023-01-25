open! Core

module Ir = struct
  module Tag = Expr_tag

  module K = struct
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
      | With_assert_log_fn of
          (* assert_input *) t * (Cint0.t -> string option) * (* value *) t
    [@@deriving sexp_of]
  end

  type 'a t = { k : K.t; tag : 'a Tag.t; max_bits : int } [@@deriving sexp_of]

  module U = struct
    type nonrec t = Any.t t
  end

  let var (v : 'a Var.Wrap.t) =
    let v = Var.Ir.unwrap v in
    let tag = Dtype.Ir.expr_tag v.u.d.dtype in
    let tag : 'a Expr_tag.t = Obj.magic (tag : Any.t Expr_tag.t) in
    let max_bits =
      match Dtype.Ir.layout v.u.d.dtype with Bits_fixed bits -> bits
    in
    { k = Var v.u; tag; max_bits }

  let untype (t : 'a t) : Any.t t = Obj.magic t
  let unwrap t = t
  let wrap t = t
  let untype' t = untype t
  let max_layout t = Layout.Bits_fixed t.max_bits
end

let cint_tag =
  Expr_tag.create ~cint_of_value:Fn.id ~value_of_cint:(fun v -> Some v)

module CBool = struct
  let tag = Cbool0.expr_tag

  let of_int i =
    assert (Ir.Tag.equal cint_tag i.Ir.tag);
    let k = i.k in
    let k =
      Ir.K.With_assert_log
        ( Le (k, Const (Cint0.of_int 1)),
          k,
          k,
          fun c -> [%string "Invalid cast CInt to CBool: %{c#Cint0}"] )
    in
    { Ir.k; tag; max_bits = 1 }

  let to_int b =
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = b.k; tag = cint_tag; max_bits = 1 }
end

module CInt = struct
  type t = Cint0.t Ir.t

  let tag = cint_tag
  let var v = Ir.var v
  let const c = { Ir.k = Const c; tag; max_bits = Cint0.bitwidth c }
  let cint i = const (Cint0.of_int i)

  (* ops *)
  let add a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Add (a.k, b.k); tag; max_bits = 1 + Int.max a.max_bits b.max_bits }

  let sub a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Sub_no_wrap (a.k, b.k); tag; max_bits = a.max_bits }

  let sub_wrap a b ~bits =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Sub_wrap (a.k, b.k, bits); tag; max_bits = bits }

  let mul a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Mul (a.k, b.k); tag; max_bits = a.max_bits + b.max_bits }

  let div a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Div (a.k, b.k); tag; max_bits = a.max_bits }

  let mod_ a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Mod (a.k, b.k); tag; max_bits = Int.min a.max_bits b.max_bits }

  let lshift a ~amt =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag amt.Ir.tag);
    {
      Ir.k = LShift (a.k, amt.k);
      tag;
      max_bits = a.max_bits + Int.pow 2 amt.max_bits - 1;
    }

  let rshift a ~amt =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag amt.Ir.tag);
    { Ir.k = LogicalRShift (a.k, amt.k); tag; max_bits = a.max_bits }

  let bit_and a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = BitAnd (a.k, b.k); tag; max_bits = Int.min a.max_bits b.max_bits }

  let bit_or a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = BitOr (a.k, b.k); tag; max_bits = Int.max a.max_bits b.max_bits }

  let bit_xor a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = BitXor (a.k, b.k); tag; max_bits = Int.max a.max_bits b.max_bits }

  let eq a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Eq (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let ne a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Ne (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let lt a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Lt (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let le a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Le (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let gt a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Gt (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let ge a b =
    assert (Ir.Tag.equal tag a.Ir.tag);
    assert (Ir.Tag.equal tag b.Ir.tag);
    { Ir.k = Ge (a.k, b.k); tag; max_bits = 1 } |> CBool.of_int

  let clip e ~bits =
    assert (Ir.Tag.equal tag e.Ir.tag);
    { Ir.k = Clip (e.k, bits); tag; max_bits = bits }

  let add_wrap a b ~bits = add a b |> clip ~bits
end

let with_assert_log ?new_max_bits ~assert_e ~val_e ~log_e log_fn =
  assert (Ir.Tag.equal CBool.tag assert_e.Ir.tag);
  let log_fn i =
    let v = Expr_tag.value_of_cint log_e.Ir.tag i |> Option.value_exn in
    log_fn v
  in
  {
    Ir.k = With_assert_log (assert_e.k, val_e.Ir.k, log_e.k, log_fn);
    tag = val_e.tag;
    max_bits = Option.value new_max_bits ~default:val_e.max_bits;
  }

module Wrap = struct
  type 'a t = 'a Ir.t [@@deriving sexp_of]

  let var v = Ir.var v
end
