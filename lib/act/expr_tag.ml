open! Core

type 'a t = {
  id : int;
  cint_of_value : 'a -> Act_ir.CInt.t;
  value_of_cint : Act_ir.CInt.t -> 'a option;
}
[@@deriving fields, sexp_of]

let next_id = ref 0

let new_id () =
  let id = !next_id in
  incr next_id;
  id

let equal a b = Int.equal a.id b.id

let create ~cint_of_value ~value_of_cint =
  { id = new_id (); cint_of_value; value_of_cint }

let cbool_expr_tag =
  create ~cint_of_value:Cbool0.to_cint ~value_of_cint:Cbool0.of_cint

let cint_expr_tag = create ~cint_of_value:Fn.id ~value_of_cint:(fun v -> Some v)
