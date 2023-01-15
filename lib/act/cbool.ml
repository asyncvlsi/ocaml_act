open! Core

include Enum.Make (struct
  include Cbool0

  let mapping = [ (false_, Cint0.of_int 0); (true_, Cint0.of_int 1) ]
end)

include Cbool0

module E = struct
  include E

  let true_ = const Cbool0.true_
  let false_ = const Cbool0.false_
  let of_bool b = match b with true -> true_ | false -> false_
  let not_ e = Expr.Ir.Not (Expr.Ir.unwrap e) |> Expr.Ir.wrap
end

module N = struct
  include N

  let toggle ?loc (var_id : Cbool0.t Var.Wrap.t) =
    Node.Wrap.assign ?loc var_id E.(var var_id |> not_)
end
