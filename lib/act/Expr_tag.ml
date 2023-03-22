open! Core

type 'a t = {
  id : int;
  cint_of_value : 'a -> Cint0.t;
  value_of_cint : Cint0.t -> 'a option;
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
let untyped_tag = Obj.magic cint_expr_tag
let untype t = Obj.magic t
