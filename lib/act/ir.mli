open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

module DType : sig
  type 'a t

  val int_ : int t
  val bool_ : bool t
  val string_ : string t
  val of_module : (module DTypeable with type t = 'a) -> 'a t
end

module type Comparable_and_hashable = sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Chan : sig
  module R : sig
    module U : Comparable_and_hashable

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
  end

  module W : sig
    module U : Comparable_and_hashable

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
  end

  type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
end

module Var : sig
  module U : Comparable_and_hashable

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> ?init:'a -> 'a DType.t -> 'a t
end

(* It is not allowed to have operation on the memory from two different locations
   in the program simultaniously. *)
module UnguardedMem : sig
  module U : Comparable_and_hashable

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a DType.t -> 'a array -> 'a t
end

module UnguardedRom : sig
  module U : Comparable_and_hashable

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a DType.t -> 'a array -> 'a t
end

module Expr : sig
  type 'a t [@@deriving sexp_of]

  val var : 'a Var.t -> 'a t
  val const : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (* ops *)
  val add : int t -> int t -> int t
  val sub : int t -> int t -> int t
  val mul : int t -> int t -> int t
  val div : int t -> int t -> int t
  val mod_ : int t -> int t -> int t
  val lshift : int t -> amt:int t -> int t
  val rshift : int t -> amt:int t -> arith:bool -> int t
  val bit_and : int t -> int t -> int t
  val bit_or : int t -> int t -> int t
  val bit_xor : int t -> int t -> int t
  val eq : int t -> int t -> bool t
  val ne : int t -> int t -> bool t
  val not_ : bool t -> bool t
end

module N : sig
  type t

  val assign : ?loc:Code_pos.t -> 'a Var.t -> 'a Expr.t -> t
  val toggle : ?loc:Code_pos.t -> bool Var.t -> t
  val incr : ?loc:Code_pos.t -> int Var.t -> t
  val read : ?loc:Code_pos.t -> 'a Chan.R.t -> 'a Var.t -> t
  val send : ?loc:Code_pos.t -> 'a Chan.W.t -> 'a Expr.t -> t
  val send' : ?loc:Code_pos.t -> 'a Chan.W.t -> 'a Var.t -> t

  (* interacting with memories *)
  val read_ug_mem :
    ?loc:Code_pos.t -> 'a UnguardedMem.t -> idx:int Expr.t -> dst:'a Var.t -> t

  val write_ug_mem :
    ?loc:Code_pos.t ->
    'a UnguardedMem.t ->
    idx:int Expr.t ->
    value:'a Expr.t ->
    t

  val write_ug_mem' :
    ?loc:Code_pos.t ->
    'a UnguardedMem.t ->
    idx:int Expr.t ->
    value:'a Var.t ->
    t

  val read_ug_rom :
    ?loc:Code_pos.t -> 'a UnguardedRom.t -> idx:int Expr.t -> dst:'a Var.t -> t

  (* phantom instructions *)
  val log : ?loc:Code_pos.t -> string Expr.t -> t
  val assert_ : ?loc:Code_pos.t -> bool Expr.t -> t

  (* control flow *)
  val seq : ?loc:Code_pos.t -> t list -> t
  val par : ?loc:Code_pos.t -> t list -> t
  val if_else : ?loc:Code_pos.t -> bool Expr.t -> t list -> t list -> t
  val loop : ?loc:Code_pos.t -> t list -> t
  val while_loop : ?loc:Code_pos.t -> bool Expr.t -> t list -> t

  val select_imm :
    ?loc:Code_pos.t -> (bool Expr.t * t) list -> else_:t option -> t
end

val block11 :
  'i1 Chan.W.t -> 'o1 Chan.R.t -> f:('i1 Chan.R.t -> 'o1 Chan.W.t -> N.t) -> N.t

(* The internal data structures. These are only meant to be constructed throguh the above interfaces. *)
module Internal_rep : sig
  module type DTypeable = sig
    type t [@@deriving sexp_of, equal]
  end

  module DType : sig
    type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }
  end

  module Chan : sig
    module U : sig
      module Id : Identifiable

      module D : sig
        type t = {
          dtype : Any.t DType.t;
          creation_code_pos : Code_pos.t;
          (* I have not come up with a way to add which direction is passive into
               the type system. These two fields help with error reporting *)
          mutable wait_readable_code_pos : Code_pos.t option;
          mutable wait_sendable_code_pos : Code_pos.t option;
        }
      end

      type t = {
        id : Id.t;
        d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
      }

      include Comparable with type t := t
      include Hashable with type t := t
    end

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val of_ir_r : 'a Chan.R.t -> U.t
    val of_ir_w : 'a Chan.W.t -> U.t
    val of_ir_ru : Chan.R.U.t -> U.t
    val of_ir_wu : Chan.W.U.t -> U.t
  end

  module Var : sig
    module U : sig
      module Id : Identifiable

      module D : sig
        type t = {
          dtype : Any.t DType.t;
          creation_code_pos : Code_pos.t;
          init : Any.t option;
        }
      end

      type t = {
        id : Id.t;
        d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
      }

      include Comparable with type t := t
      include Hashable with type t := t
    end

    type 'a t = { u : U.t } [@@deriving sexp_of]
  end

  module Unguarded_mem : sig
    module Id : Identifiable

    module D : sig
      type t = {
        dtype : Any.t DType.t;
        creation_code_pos : Code_pos.t;
        init : Any.t array;
        kind : [ `Mem | `Rom ];
      }
    end

    type t = {
      id : Id.t;
      d : (D.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
    }

    include Comparable with type t := t
    include Hashable with type t := t
  end

  module Expr : sig
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

    module U : sig
      type nonrec t = Any.t t
    end

    val untype : 'a t -> U.t
  end

  module N : sig
    type t =
      | Assign of Code_pos.t * Var.U.t * Expr.U.t
      | Log of Code_pos.t * string Expr.t
      | Assert of Code_pos.t * bool Expr.t
      | Seq of Code_pos.t * t list
      | Par of Code_pos.t * t list
      | Read of Code_pos.t * Chan.U.t * Var.U.t
      | Send of Code_pos.t * Chan.U.t * Expr.U.t
      | Loop of Code_pos.t * t
      | WhileLoop of Code_pos.t * bool Expr.t * t
      | SelectImm of Code_pos.t * (bool Expr.t * t) list * t option
      | ReadUGMem of Code_pos.t * Unguarded_mem.t * int Expr.t * Var.U.t
      | WriteUGMem of Code_pos.t * Unguarded_mem.t * int Expr.t * Expr.U.t

    val of_ir : N.t -> t
  end
end
