open! Core

module T = struct
  type 'a t =
    | Var : 'a Var.Ir.t -> 'a t
    | Const : 'a * Layout.t -> 'a t
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
    | Magic_enum_eq : Any.t t * Any.t t -> Cbool0.t t
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
    | Const (_, layout) -> layout
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
    | Magic_enum_eq (_, _) -> Bits_fixed 1
    | Ne (_, _) -> Bits_fixed 1
    | Not _ -> Bits_fixed 1
end

module CBool_ = struct
  include T

  let var v = Wrap.var v
  let true_ = Const (Cbool0.true_, Bits_fixed 1)
  let false_ = Const (Cbool0.false_, Bits_fixed 1)
  let not_ a = Not a
end

module CInt_ = struct
  include T

  let var v = Wrap.var v
  let const c = Const (c, Bits_fixed (Cint0.bitwidth c))
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
end

module Ir = struct
  include T
  module U = Wrap.U

  let untype t = Wrap.untype t
  let unwrap t = t
  let wrap t = t
  let untype' t = untype t
  let max_layout t = Wrap.max_layout t
end
