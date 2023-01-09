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

module Chan : sig
  module R : sig
    module U : sig
      type t [@@deriving sexp_of, compare, equal, hash]

      include Comparable with type t := t
      include Hashable with type t := t
    end

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
  end

  module W : sig
    module U : sig
      type t [@@deriving sexp_of, compare, equal, hash]

      include Comparable with type t := t
      include Hashable with type t := t
    end

    type 'a t = { u : U.t } [@@deriving sexp_of]

    val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
  end

  type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> 'a DType.t -> 'a t
end

module Var : sig
  module U : sig
    type t [@@deriving sexp_of]
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  val create : ?loc:Code_pos.t -> ?init:'a -> 'a DType.t -> 'a t
end

(* It is not allowed to have operation on the memory from two different locations
   in the program simultaniously. *)
module UnguardedMem : sig
  type 'a t

  val create : ?loc:Code_pos.t -> 'a DType.t -> len:int -> default:'a -> 'a t

  val create_init :
    ?loc:Code_pos.t -> 'a DType.t -> len:int -> f:(int -> 'a) -> 'a t

  val create_init_array : ?loc:Code_pos.t -> 'a DType.t -> 'a array -> 'a t
end

module UnguardedRom : sig
  type 'a t

  val create : ?loc:Code_pos.t -> 'a DType.t -> len:int -> default:'a -> 'a t

  val create_init :
    ?loc:Code_pos.t -> 'a DType.t -> len:int -> f:(int -> 'a) -> 'a t

  val create_init_array : ?loc:Code_pos.t -> 'a DType.t -> 'a array -> 'a t
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

module Sim : sig
  type t [@@deriving sexp_of]

  val create :
    N.t ->
    user_sendable_ports:Chan.W.U.t list ->
    user_readable_ports:Chan.R.U.t list ->
    t

  val wait :
    t -> ?max_steps:int -> ?line_numbers:bool -> unit -> unit Or_error.t

  val wait' : t -> ?max_steps:int -> unit -> unit
  val send : t -> ?loc:Code_pos.t -> 'a Chan.W.t -> 'a -> unit
  val read : t -> ?loc:Code_pos.t -> 'a Chan.R.t -> 'a -> unit
end

val block11 :
  'i1 Chan.W.t -> 'o1 Chan.R.t -> f:('i1 Chan.R.t -> 'o1 Chan.W.t -> N.t) -> N.t
