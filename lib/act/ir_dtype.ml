open! Core

type 'a t = {
  equal : 'a -> 'a -> bool;
  sexp_of_t : 'a -> Sexp.t;
  max_layout_of : 'a -> Ir_layout.t;
  cint_of : 'a -> Cint0.t;
  of_cint : Cint0.t -> 'a option;
  sexp_of_cint : Cint0.t -> Sexp.t;
  of_cint_assert_expr_fn : unit Ir_expr0.t;
  layout : Ir_layout.t;
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

let fits_into_dtype ~into (layout : Ir_layout.t) =
  match (into.layout, layout) with
  | Bits_fixed into, Bits_fixed layout -> layout <= into

let fits_value t value = fits_into_dtype (max_layout_of t value) ~into:t
let expr_tag t = t.expr_tag

let cbool_dtype =
  create ~equal:Cbool0.equal ~sexp_of_t:Cbool0.sexp_of_t
    ~max_layout_of:(fun _ -> Bits_fixed 1)
    ~cint_of:(fun t -> Cbool0.to_cint t)
    ~of_cint_assert_expr_fn:(Ir_expr0.Le (Var (), Const Cint0.one))
    ~of_cint:Cbool0.of_cint ~layout:(Bits_fixed 1)
    ~expr_tag:Expr_tag.cbool_expr_tag

let cint_dtype ~bits =
  create ~equal:Cint0.equal ~sexp_of_t:Cint0.sexp_of_t
    ~max_layout_of:(fun v -> Bits_fixed (Cint0.bitwidth v))
    ~cint_of:(fun v -> v)
    ~of_cint:(fun v -> Some v)
    ~of_cint_assert_expr_fn:(Ir_expr0.Const Cint0.one) ~layout:(Bits_fixed bits)
    ~expr_tag:Expr_tag.cint_expr_tag

let untype t = Obj.magic t
