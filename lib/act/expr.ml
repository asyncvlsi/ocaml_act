open! Core

module Ir = struct
  module Tag = Expr_tag
  module K = Expr0

  type 'a t = { k : Var.Ir.U.t K.t; tag : 'a Tag.t; max_bits : int }
  [@@deriving sexp_of]

  module U = struct
    type nonrec t = Any.t t [@@deriving sexp_of]
  end

  let var (v : 'a Var.t) =
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

  type 'a outer = 'a t

  let cint_tag =
    Expr_tag.create ~cint_of_value:Fn.id ~value_of_cint:(fun v -> Some v)

  let cbool_tag = Cbool0.expr_tag

  let bool_of_int i =
    assert (Tag.equal cint_tag i.tag);
    let k = i.k in
    let k =
      K.With_assert_log
        ( Le (k, Const (Cint0.of_int 1)),
          k,
          k,
          fun c -> [%string "Invalid cast CInt to CBool: %{c#Cint0}"] )
    in
    { k; tag = cbool_tag; max_bits = 1 }

  let int_of_bool b =
    assert (Tag.equal cbool_tag b.tag);
    { k = b.k; tag = cint_tag; max_bits = 1 }

  let of_cint c = { k = Const c; tag = cint_tag; max_bits = Cint0.bitwidth c }
  let of_int i = of_cint (Cint0.of_int i)
  let of_bool b = Cbool0.of_bool b |> Cbool0.to_cint |> of_cint |> bool_of_int
  let zero = of_int 0
  let one = of_int 1
  let two = of_int 2
  let three = of_int 3
  let four = of_int 4
  let five = of_int 5
  let true_ = of_bool true
  let false_ = of_bool false

  let not_ b =
    assert (Tag.equal cbool_tag b.tag);
    { k = Eq (b.k, Const Cint0.zero); tag = cbool_tag; max_bits = 1 }

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
    { k = LogicalRShift (a.k, amt.k); tag = cint_tag; max_bits = a.max_bits }

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

  let clip e ~bits =
    assert (Tag.equal cint_tag e.tag);
    { k = Clip (e.k, bits); tag = cint_tag; max_bits = bits }

  let add_wrap a b ~bits = add a b |> clip ~bits

  let with_assert_log ?new_max_bits ~assert_e ~val_e ~log_e log_fn =
    assert (Tag.equal cbool_tag assert_e.tag);
    let log_fn i =
      let v = Expr_tag.value_of_cint log_e.tag i |> Option.value_exn in
      log_fn v
    in
    {
      k = With_assert_log (assert_e.k, val_e.k, log_e.k, log_fn);
      tag = val_e.tag;
      max_bits = Option.value new_max_bits ~default:val_e.max_bits;
    }

  let var v = var v
end

include Ir
