open! Core
open! Core
module Tag = Expr_tag
module K = Act_ir.Ir.Expr

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
  { k = LogicalRShift (a.k, amt.k); tag = cint_tag; max_bits = a.max_bits }

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
  module Assert = struct
    type t = {
      guards : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t list;
      cond : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
      log_e : Act_ir.Ir.Var.t Act_ir.Ir.Expr.t;
      f : Act_ir.CInt.t -> string;
    }
    [@@deriving sexp_of]
  end

  let unwrap t = ([], t.k)
  let tag t = t.tag
  let max_bits t = t.max_bits
  let wrap k tag max_bits = { k; tag; max_bits }

  let with_set_tag_and_max_bits { k; tag = _; max_bits = old_max_bits } tag
      max_bits =
    { k; tag; max_bits = Int.min old_max_bits max_bits }
end
