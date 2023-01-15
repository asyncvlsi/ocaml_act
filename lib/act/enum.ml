open! Core

module type E_S = sig
  type elt
  type t = elt Expr.Wrap.t

  val var : elt Var.Wrap.t -> elt Expr.Wrap.t
  val const : elt -> elt Expr.Wrap.t
  val eq : elt Expr.Wrap.t -> elt Expr.Wrap.t -> Cbool0.t Expr.Wrap.t
end

module type N_S = sig
  type elt
  type t = Node.Wrap.t

  val match_ : elt Var.Wrap.t -> f:(elt -> t) -> t
end

module type S = sig
  type t

  include Sexpable with type t := t
  include Comparable with type t := t
  include Hashable with type t := t

  val dtype : t Dtype.Wrap.t
  val bitwidth : t -> int

  module E : E_S with type elt := t
  module N : N_S with type elt := t
end

module Make (X : sig
  type t [@@deriving sexp, hash, compare, equal]

  val mapping : (t * Cint0.t) list
end) : S with type t := X.t = struct
  module T = struct
    type t = X.t [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let to_int t = List.find_exn X.mapping ~f:(fun (op, _) -> X.equal op t) |> snd

  let of_int i =
    List.find_exn X.mapping ~f:(fun (_, op_code) -> Cint0.equal op_code i)
    |> fst

  let bitwidth t = to_int t |> Cint0.bitwidth

  let max_bitwidth =
    List.map X.mapping ~f:(fun (t, _) -> bitwidth t)
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn

  let dtype =
    Dtype.Wrap.create ~equal ~sexp_of_t:X.sexp_of_t
      ~max_layout_of:(fun t -> Bits_fixed (bitwidth t))
      ~layout:(Bits_fixed max_bitwidth)

  module E = struct
    type t = X.t Expr.Wrap.t

    let var v = Expr.Wrap.var v

    let const c =
      Expr.Ir.magic_EnumOfCInt (Expr.CInt_.const (to_int c)) ~f:of_int

    let eq a b =
      let a = Expr.Ir.magic_EnumToCInt a ~f:to_int in
      let b = Expr.Ir.magic_EnumToCInt b ~f:to_int in
      Expr.CInt_.eq a b
  end

  module N = struct
    type t = Node.Wrap.t

    let match_ var0 ~f =
      Node.Wrap.select_imm ~else_:None
        (List.map X.mapping ~f:(fun (op, _) ->
             let guard = E.(eq (const op) (var var0)) in
             (guard, f op)))
  end
end
