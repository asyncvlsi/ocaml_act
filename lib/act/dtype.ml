open! Core

type 'a t = {
  equal : 'a -> 'a -> bool;
  sexp_of_t : 'a -> Sexp.t;
  max_layout_of : 'a -> Layout.t;
  cint_of : 'a -> Cint0.t;
  of_cint : Cint0.t -> 'a option;
  layout : Layout.t;
  expr_tag : 'a Expr_tag.t;
}

module Ir = struct
  type 'a outer = 'a t
  type nonrec 'a t = 'a t

  let create ~equal ~sexp_of_t ~max_layout_of ~cint_of ~of_cint ~layout
      ~expr_tag =
    { equal; sexp_of_t; max_layout_of; cint_of; of_cint; layout; expr_tag }

  let dummy_val =
    {
      equal = (fun _ _ -> failwith "Cant call functions on DType.dummy_val");
      sexp_of_t = (fun _ -> failwith "Cant call functions on DType.dummy_val");
      max_layout_of =
        (fun _ -> failwith "Cant call functions on DType.dummy_val");
      cint_of = (fun _ -> failwith "Cant call functions on DType.dummy_val");
      of_cint = (fun _ -> failwith "Cant call functions on DType.dummy_val");
      layout = Bits_fixed 0;
      expr_tag =
        Expr_tag.create
          ~value_of_cint:(fun _ ->
            failwith "Cant call functions on DType.dummy_val")
          ~cint_of_value:(fun _ ->
            failwith "Cant call functions on DType.dummy_val");
    }

  let layout t = t.layout
  let untype (t : 'a t) : Any.t t = Obj.magic t
  let untype' t = untype t
  let unwrap t = t
  let equal_ (t : 'a t) (a : 'a) (b : 'a) = t.equal a b
  let sexp_of_t_ (t : 'a t) (a : 'a) = t.sexp_of_t a
  let cint_of_value (t : 'a t) (a : 'a) = t.cint_of a
  let value_of_cint_exn (t : 'a t) i = t.of_cint i |> Option.value_exn
  let equal_fn (t : 'a t) = Staged.stage t.equal
  let sexp_of_t_fn (t : 'a t) = Staged.stage t.sexp_of_t
  let max_layout_of t a = t.max_layout_of a

  let fits_into_dtype ~into (layout : Layout.t) =
    match (into.layout, layout) with
    | Bits_fixed into, Bits_fixed layout -> layout <= into

  let fits_value t value = fits_into_dtype (max_layout_of t value) ~into:t
  let expr_tag t = t.expr_tag
end
