open! Core

module T = struct
  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : Cint0.t -> Cint0.t t
    | Add : Cint0.t t * Cint0.t t -> Cint0.t t
    | Sub : Cint0.t t * Cint0.t t -> Cint0.t t
    | Mul : Cint0.t t * Cint0.t t -> Cint0.t t
    | Div : Cint0.t t * Cint0.t t -> Cint0.t t
    | Mod : Cint0.t t * Cint0.t t -> Cint0.t t
    | LShift : Cint0.t t * Cint0.t t -> Cint0.t t
    | LogicalRShift : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitAnd : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitOr : Cint0.t t * Cint0.t t -> Cint0.t t
    | BitXor : Cint0.t t * Cint0.t t -> Cint0.t t
    | Eq : Cint0.t t * Cint0.t t -> Cbool0.t t
    | Ne : Cint0.t t * Cint0.t t -> Cbool0.t t
    | Not : Cbool0.t t -> Cbool0.t t
    | Clip : Cint0.t t * int -> Cint0.t t
    | Add_wrap : Cint0.t t * Cint0.t t * int -> Cint0.t t
    | Sub_wrap : Cint0.t t * Cint0.t t * int -> Cint0.t t
    | Magic_EnumToCInt : Any.t t * (Any.t -> Cint0.t) -> Cint0.t t
    | Magic_EnumOfCInt : Cint0.t t * (Cint0.t -> 'a) -> 'a t
  [@@deriving sexp_of]
end

module Wrap = struct
  include T

  (* internal *)
  let untype t : Any.t t = Obj.magic t

  module U = struct
    type nonrec t = Any.t t
  end

  (* main operations *)
  let var v = Var (Var.Ir.unwrap v)

  module Layout = struct
    include Layout

    let imax a b =
      match (a, b) with Bits_fixed a, Bits_fixed b -> Bits_fixed (Int.max a b)

    let imin a b =
      match (a, b) with Bits_fixed a, Bits_fixed b -> Bits_fixed (Int.min a b)

    let iadd a b =
      match (a, b) with Bits_fixed a, Bits_fixed b -> Bits_fixed (a + b)

    let iadd1 a = match a with Bits_fixed a -> Bits_fixed (a + 1)

    let ilshift a amt =
      match (a, amt) with
      | Bits_fixed a, Bits_fixed amt -> Bits_fixed (a + Int.pow 2 amt - 1)
  end

  let rec max_layout : type a. a t -> Layout.t = function
    | Var var_id -> Dtype.Ir.layout var_id.u.d.dtype
    | Const c -> Bits_fixed (Cint0.bitwidth c)
    | Add (a, b) -> Layout.(iadd1 (imax (max_layout a) (max_layout b)))
    | Sub (a, _) -> (* this is not allowed to underflow *) max_layout a
    | Mul (a, b) -> Layout.(iadd (max_layout a) (max_layout b))
    | Div (a, _) -> max_layout a
    | Mod (a, b) -> Layout.imin (max_layout a) (max_layout b)
    | LShift (a, b) -> Layout.ilshift (max_layout a) (max_layout b)
    | LogicalRShift (a, _) -> max_layout a
    | BitAnd (a, b) -> Layout.imin (max_layout a) (max_layout b)
    | BitOr (a, b) -> Layout.imax (max_layout a) (max_layout b)
    | BitXor (a, b) -> Layout.imax (max_layout a) (max_layout b)
    | Eq (_, _) -> Bits_fixed 1
    | Ne (_, _) -> Bits_fixed 1
    | Not _ -> Bits_fixed 1
    | Magic_EnumToCInt (c, _) -> max_layout c
    | Magic_EnumOfCInt (c, _) -> max_layout c
    | Clip (e, bits) -> Layout.imin (max_layout e) (Bits_fixed bits)
    | Add_wrap (a, b, bits) ->
        let add_layout = Layout.(iadd1 (imax (max_layout a) (max_layout b))) in
        Layout.imin add_layout (Bits_fixed bits)
    | Sub_wrap (_, _, bits) -> Bits_fixed bits
end

module CInt_ = struct
  include T

  let var v = Wrap.var v
  let const c = Const c
  let cint i = const (Cint0.of_int i)

  (* ops *)
  let add a b = Add (a, b)
  let sub a b = Sub (a, b)
  let mul a b = Mul (a, b)
  let div a b = Div (a, b)
  let mod_ a b = Mod (a, b)
  let lshift a ~amt = LShift (a, amt)
  let rshift a ~amt = LogicalRShift (a, amt)
  let bit_and a b = BitAnd (a, b)
  let bit_or a b = BitOr (a, b)
  let bit_xor a b = BitXor (a, b)
  let eq a b = Eq (a, b)
  let ne a b = Ne (a, b)
  let clip e ~bits = Clip (e, bits)
  let add_wrap a b ~bits = Add_wrap (a, b, bits)
  let sub_wrap a b ~bits = Sub_wrap (a, b, bits)
end

module Ir = struct
  include T
  module U = Wrap.U

  let untype t = Wrap.untype t
  let unwrap t = t
  let wrap t = t
  let untype' t = untype t
  let max_layout t = Wrap.max_layout t

  let magic_EnumToCInt (enum : 'a t) ~(f : 'a -> Cint0.t) =
    let enum : Any.t t = Obj.magic enum in
    let f : Any.t -> Cint0.t = Obj.magic f in
    Magic_EnumToCInt (enum, f)

  let magic_EnumOfCInt cint ~f = Magic_EnumOfCInt (cint, f)
end
