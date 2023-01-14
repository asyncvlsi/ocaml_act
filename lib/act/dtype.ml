open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

module T = struct
  type 'a t = {
    equal : 'a -> 'a -> bool;
    sexp_of_t : 'a -> Sexp.t;
    width : Width.t;
  }
end

include T

let int32 = { equal = Cint.equal; sexp_of_t = Cint.sexp_of_t; width = Fixed 32 }
let bool_ = { equal = Bool.equal; sexp_of_t = Bool.sexp_of_t; width = Fixed 1 }

let string_ =
  { equal = String.equal; sexp_of_t = String.sexp_of_t; width = Unlimited }

module Ir = struct
  include T

  type 'a outer = 'a t

  let untype (t : 'a t) : Any.t t = Obj.magic t
  let untype' (t : 'a outer) : Any.t t = untype t
  let unwrap t = t
  let equal_ (t : 'a t) (a : 'a) (b : 'a) = t.equal (Obj.magic a) (Obj.magic b)
  let sexp_of_t_ (t : 'a t) (a : 'a) = t.sexp_of_t (Obj.magic a)
  let equal_fn (t : 'a t) = Staged.stage (Obj.magic t.equal : 'a -> 'a -> bool)

  let sexp_of_t_fn (t : 'a t) =
    Staged.stage (Obj.magic t.sexp_of_t : 'a -> Sexp.t)

  let width t = t.width
end
