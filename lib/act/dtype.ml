open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end


module T = struct
  type 'a t = {
    equal : 'a -> 'a -> bool;
    sexp_of_t : 'a -> Sexp.t;
    max_layout_of : 'a -> Layout.t;
    layout : Layout.t;
  }
end

include T

let int_ ~bits =
  {
    equal = Cint.equal;
    sexp_of_t = Cint.sexp_of_t;
    max_layout_of = (fun v -> Layout.Fixed (Cint.bitwidth v));
    layout = Fixed bits;
  }

let int32 = int_ ~bits:32

let bool_ =
  {
    equal = Cbool.equal;
    sexp_of_t = Cbool.sexp_of_t;
    max_layout_of = (fun _ -> Fixed 1);
    layout = Fixed 1;
  }

let string_ =
  {
    equal = String.equal;
    sexp_of_t = String.sexp_of_t;
    max_layout_of = (fun _ -> Unknown);
    layout = Unknown;
  }

module Ir = struct
  include T

  type 'a outer = 'a t

  let layout t = t.layout
  let untype (t : 'a t) : Any.t t = Obj.magic t
  let untype' (t : 'a outer) : Any.t t = untype t
  let unwrap t = t
  let equal_ (t : 'a t) (a : 'a) (b : 'a) = t.equal (Obj.magic a) (Obj.magic b)
  let sexp_of_t_ (t : 'a t) (a : 'a) = t.sexp_of_t (Obj.magic a)
  let equal_fn (t : 'a t) = Staged.stage (Obj.magic t.equal : 'a -> 'a -> bool)

  let sexp_of_t_fn (t : 'a t) =
    Staged.stage (Obj.magic t.sexp_of_t : 'a -> Sexp.t)

  let max_layout_of t a = t.max_layout_of a

  let fits_value t ~value =
    match (t.layout, value) with
    | Fixed t, Layout.Fixed v -> v <= t
    | Unknown, Fixed _ ->
        failwith
          "unreachable: A dtype of kind unknown should never be asked if it \
           fits a value of fixed width"
    | Fixed _, Unknown -> false
    | Unknown, Unknown -> true
end
