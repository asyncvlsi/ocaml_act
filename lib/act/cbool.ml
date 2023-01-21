open! Core

include Enum.Make (struct
  include Cbool0

  let mapping = [ (false, Cint0.of_int 0); (true, Cint0.of_int 1) ]
end)

include Cbool0

module E = struct
  include E

  let true_ = const true
  let false_ = const false
  let not_ e = Expr.Ir.Not (Expr.Ir.unwrap e) |> Expr.Ir.wrap
  let eq _ _ = failwith "TODO"
  let ne _ _ = failwith "TODO"
  let and_ _ _ = failwith "TODO"
  let or_ _ _ = failwith "TODO"
  let xor_ _ _ = failwith "TODO"
end

module N = struct
  include N

  let toggle ?loc (var_id : Cbool0.t Var.Wrap.t) =
    Node.Wrap.assign ?loc var_id E.(var var_id |> not_)
  let set_true ?loc var_id =
    Node.Wrap.assign ?loc var_id E.true_
  let set_false ?loc var_id =
    Node.Wrap.assign ?loc var_id E.false_
end
