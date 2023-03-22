open! Core

type 'a t = {
  equal : 'a -> 'a -> bool;
  sexp_of_t : 'a -> Sexp.t;
  max_layout_of : 'a -> Layout.t;
  cint_of : 'a -> Act_ir.CInt.t;
  of_cint : Act_ir.CInt.t -> 'a option;
  sexp_of_cint : Act_ir.CInt.t -> Sexp.t;
  of_cint_assert_expr_fn : unit Act_ir.Expr.t;
  layout : Layout.t;
  expr_tag : 'a Expr_tag.t;
}
[@@deriving sexp_of]

let create ~equal ~sexp_of_t ~max_layout_of ~cint_of ~of_cint
    ~of_cint_assert_expr_fn ~layout ~expr_tag =
  {
    equal;
    sexp_of_t;
    max_layout_of;
    cint_of;
    of_cint;
    of_cint_assert_expr_fn;
    layout;
    expr_tag;
    sexp_of_cint = (fun i -> Option.sexp_of_t sexp_of_t (of_cint i));
  }

let dummy_val =
  {
    equal = (fun _ _ -> failwith "Cant call functions on Dtype.dummy_val");
    sexp_of_t = (fun _ -> failwith "Cant call functions on Dtype.dummy_val");
    sexp_of_cint = (fun _ -> failwith "Cant call functions on Dtype.dummy_val");
    max_layout_of = (fun _ -> failwith "Cant call functions on Dtype.dummy_val");
    cint_of = (fun _ -> failwith "Cant call functions on Dtype.dummy_val");
    of_cint = (fun _ -> failwith "Cant call functions on Dtype.dummy_val");
    of_cint_assert_expr_fn = Var ();
    layout = Bits_fixed 0;
    expr_tag =
      Expr_tag.create
        ~value_of_cint:(fun _ ->
          failwith "Cant call functions on Dtype.dummy_val")
        ~cint_of_value:(fun _ ->
          failwith "Cant call functions on Dtype.dummy_val");
  }

let layout t = t.layout
let equal_ (t : 'a t) (a : 'a) (b : 'a) = t.equal a b
let sexp_of_t_ (t : 'a t) (a : 'a) = t.sexp_of_t a
let cint_of_value (t : 'a t) (a : 'a) = t.cint_of a
let of_cint_assert_expr_fn t = t.of_cint_assert_expr_fn
let value_of_cint_exn (t : 'a t) i = t.of_cint i |> Option.value_exn
let equal_fn (t : 'a t) = Staged.stage t.equal
let sexp_of_t_fn (t : 'a t) = Staged.stage t.sexp_of_t
let max_layout_of t a = t.max_layout_of a

let fits_into_dtype ~into (layout : Layout.t) =
  match (into.layout, layout) with
  | Bits_fixed into, Bits_fixed layout -> layout <= into

let fits_value t value = fits_into_dtype (max_layout_of t value) ~into:t
let expr_tag t = t.expr_tag

let cbool_dtype =
  create ~equal:Act_ir.CBool.equal ~sexp_of_t:Act_ir.CBool.sexp_of_t
    ~max_layout_of:(fun _ -> Bits_fixed 1)
    ~cint_of:(fun t -> Act_ir.CBool.to_cint t)
    ~of_cint_assert_expr_fn:(Act_ir.Expr.Le (Var (), Const Act_ir.CInt.one))
    ~of_cint:Act_ir.CBool.of_cint ~layout:(Bits_fixed 1)
    ~expr_tag:Expr_tag.cbool_expr_tag

let cint_dtype ~bits =
  create ~equal:Act_ir.CInt.equal ~sexp_of_t:Act_ir.CInt.sexp_of_t
    ~max_layout_of:(fun v -> Bits_fixed (Act_ir.CInt.bitwidth v))
    ~cint_of:(fun v -> v)
    ~of_cint:(fun v -> Some v)
    ~of_cint_assert_expr_fn:(Act_ir.Expr.Const Act_ir.CInt.one)
    ~layout:(Bits_fixed bits) ~expr_tag:Expr_tag.cint_expr_tag

let untype t = Obj.magic t
