open! Core

module Any = Any
module Vec = Vec

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

module DType = struct
  type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }

  let create ~(equal : 'a -> 'a -> bool) ~(sexp_of_t : 'a -> Sexp.t) : 'a t =
    { equal = Obj.magic equal; sexp_of_t = Obj.magic sexp_of_t }

  let of_module (type a) (module M : DTypeable with type t = a) : a t =
    create ~equal:M.equal ~sexp_of_t:M.sexp_of_t

  let int_ = of_module (module Int)
  let bool_ = of_module (module Bool)
  let string_ = of_module (module String)
  let untype (t : 'a t) : Any.t t = Obj.magic t
end

module Code_pos = Code_pos
module type Comparable_and_hashable = sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Id : sig
  include Identifiable

  val create : unit -> t
end = struct
  include Int

  let next_int = ref 0

  let create () =
    let id = !next_int in
    incr next_int;
    id
end

module Chan_ = struct
  module U = struct
    module Id = Id

    module D = struct
      type t = {
        dtype : Any.t DType.t;
        creation_code_pos : Code_pos.t;
        (* I have not come up with a way to add which direction is passive into
             the type system. These two fields help with error reporting *)
        mutable wait_readable_code_pos : Code_pos.t option;
        mutable wait_sendable_code_pos : Code_pos.t option;
      }
    end

    module T = struct
      type t = {
        id : Id.t;
        d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let create dtype creation_code_pos =
      let id = Id.create () in
      let dtype : Any.t DType.t = Obj.magic dtype in
      let d =
        {
          D.dtype;
          creation_code_pos;
          wait_readable_code_pos = None;
          wait_sendable_code_pos = None;
        }
      in
      { id; d }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype (Code_pos.value_or_psite loc) }
end

module Chan = struct
  module R = struct
    module U = Chan_.U

    type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

    let create = Chan_.create
  end

  module W = struct
    module U = Chan_.U

    type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

    let create = Chan_.create
  end

  type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

  let create ?loc (dtype : 'a DType.t) : 'a t =
    let c = Chan_.create ?loc dtype in
    { r = c; w = c }
end

module Var = struct
  module U = struct
    module Id = Id

    module D = struct
      type t = {
        dtype : Any.t DType.t;
        creation_code_pos : Code_pos.t;
        init : Any.t option;
      }
    end

    module T = struct
      type t = {
        id : Id.t;
        d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let create dtype init creation_code_pos =
      let id = Id.create () in
      let dtype = DType.untype dtype in
      let init = Any.Option.of_magic init in
      { id; d = { dtype; creation_code_pos; init } }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc ?init (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype init (Code_pos.value_or_psite loc) }
end

module UnguardedMemRom_u = struct
  module Id = Id

  module D = struct
    type t = {
      dtype :
        (Any.t DType.t
        [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      creation_code_pos :
        (Code_pos.t
        [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      init :
        (Any.t array
        [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      kind :
        ([ `Mem | `Rom ]
        [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
    }
  end

  module T = struct
    type t = {
      id : Id.t;
      d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
    }
    [@@deriving hash, compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create dtype init creation_code_pos kind =
    let id = Id.create () in
    let dtype : Any.t DType.t = Obj.magic dtype in
    { id; d = { dtype; creation_code_pos; init; kind } }
end

module UnguardedMem = struct
  module U = UnguardedMemRom_u

  type 'a t = { u : U.t } [@@deriving sexp]

  let create ?loc dtype arr =
    { u = U.create dtype (Obj.magic arr) (Code_pos.value_or_psite loc) `Rom }
end

module UnguardedRom = struct
  module U = UnguardedMemRom_u

  type 'a t = { u : U.t } [@@deriving sexp]

  let create ?loc dtype arr =
    { u = U.create dtype (Obj.magic arr) (Code_pos.value_or_psite loc) `Rom }
end

module Expr = struct
  type 'a t =
    | Var : 'a Var.t -> 'a t
    | Const : 'a -> 'a t
    | Map : Any.t t * (Any.t -> 'a) -> 'a t
    | Add : int t * int t -> int t
    | Sub : int t * int t -> int t
    | Mul : int t * int t -> int t
    | Div : int t * int t -> int t
    | Mod : int t * int t -> int t
    | LShift : int t * int t -> int t
    | LogicalRShift : int t * int t -> int t
    | ArithRShift : int t * int t -> int t
    | BitAnd : int t * int t -> int t
    | BitOr : int t * int t -> int t
    | BitXor : int t * int t -> int t
    | Eq : int t * int t -> bool t
    | Ne : int t * int t -> bool t
    | Not : bool t -> bool t
  [@@deriving sexp_of]

  (* internal *)
  let untype t : Any.t t = Obj.magic t

  module U = struct
    type nonrec t = Any.t t
  end

  (* main operations *)
  let var v = Var v
  let const c = Const c
  let map (e : 'a t) ~(f : 'a -> 'b) = Map (untype e, Obj.magic f)

  (* ops *)
  let add a b = Add (a, b)
  let sub a b = Sub (a, b)
  let mul a b = Mul (a, b)
  let div a b = Div (a, b)
  let mod_ a b = Mod (a, b)
  let lshift a ~amt = LShift (a, amt)

  let rshift a ~amt ~arith =
    if arith then ArithRShift (a, amt) else LogicalRShift (a, amt)

  let bit_and a b = BitAnd (a, b)
  let bit_or a b = BitOr (a, b)
  let bit_xor a b = BitXor (a, b)
  let eq a b = Eq (a, b)
  let ne a b = Ne (a, b)
  let not_ a = Not a
end

module N = struct
  type t =
    | Assign of Code_pos.t * Var.U.t * Expr.U.t
    | Log of Code_pos.t * string Expr.t
    | Assert of Code_pos.t * bool Expr.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | Read of Code_pos.t * Chan_.U.t * Var.U.t
    | Send of Code_pos.t * Chan_.U.t * Expr.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * bool Expr.t * t
    | SelectImm of Code_pos.t * (bool Expr.t * t) list * t option
    | ReadUGMem of Code_pos.t * UnguardedMem.U.t * int Expr.t * Var.U.t
    | WriteUGMem of Code_pos.t * UnguardedMem.U.t * int Expr.t * Expr.U.t

  let assign ?loc var_id expr =
    Assign (Code_pos.value_or_psite loc, var_id.Var.u, Expr.untype expr)

  let toggle ?loc var_id = assign ?loc var_id Expr.(var var_id |> not_)
  let incr ?loc var_id = assign ?loc var_id Expr.(var var_id |> add (const 1))

  let read ?loc chan_id var_id =
    Read (Code_pos.value_or_psite loc, chan_id.Chan_.u, var_id.Var.u)

  let send ?loc chan_id (expr : 'a Expr.t) =
    Send (Code_pos.value_or_psite loc, chan_id.Chan_.u, Expr.untype expr)

  let send' ?loc chan_id var_id = send ?loc chan_id Expr.(var var_id)

  (* interacting with memories *)
  let read_ug_mem ?loc (mem : 'a UnguardedMem.t) ~idx ~(dst : 'a Var.t) =
    ReadUGMem (Code_pos.value_or_psite loc, mem.u, idx, dst.u)

  let write_ug_mem ?loc (mem : 'a UnguardedMem.t) ~idx ~(value : 'a Expr.t) =
    WriteUGMem (Code_pos.value_or_psite loc, mem.u, idx, Expr.untype value)

  let write_ug_mem' ?loc (mem : 'a UnguardedMem.t) ~idx ~(value : 'a Var.t) =
    write_ug_mem ?loc mem ~idx ~value:Expr.(var value)

  let read_ug_rom ?loc (rom : 'a UnguardedRom.t) ~idx ~(dst : 'a Var.t) =
    ReadUGMem (Code_pos.value_or_psite loc, rom.u, idx, dst.u)

  let log ?loc expr = Log (Code_pos.value_or_psite loc, expr)

  let assert_ ?loc expr =
    Assert (Option.value loc ~default:(Code_pos.psite ()), expr)

  let seq ?loc l = Seq (Code_pos.value_or_psite loc, l)
  let par ?loc l = Par (Code_pos.value_or_psite loc, l)

  let if_else ?loc expr t_br f_br =
    SelectImm
      ( Code_pos.value_or_psite loc,
        [ (expr, seq ?loc t_br) ],
        Some (seq ?loc f_br) )

  let loop ?loc t = Loop (Code_pos.value_or_psite loc, seq ?loc t)

  let while_loop ?loc expr t =
    WhileLoop (Code_pos.value_or_psite loc, expr, seq ?loc t)

  let select_imm ?loc branches ~else_ =
    SelectImm (Code_pos.value_or_psite loc, branches, else_)
end

let block11 i1 o1 ~f = f i1 o1

module Internal_rep = struct
  module type DTypeable = DTypeable

  module DType = DType

  module Chan = struct
    include Chan_

    let of_ir_r t = t.u
    let of_ir_w t = t.u
    let of_ir_ru t = t
    let of_ir_wu t = t
  end

  module Var = Var
  module Unguarded_mem = UnguardedMemRom_u
  module Expr = Expr

  module N = struct
    include N

    let of_ir t = t
  end
end
