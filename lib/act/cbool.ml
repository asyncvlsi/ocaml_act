open! Core
include Cbool0

let dtype =
  Dtype.Wrap.create ~equal ~sexp_of_t
    ~max_layout_of:(fun _ -> Bits_fixed 1)
    ~layout:(Bits_fixed 1)

module E = struct
  type t = Cbool0.t Expr.Wrap.t

  include Expr.CBool_
end

module N = struct
  type t = Node.Wrap.t

  let toggle ?loc var_id = Node.Wrap.assign ?loc var_id E.(var var_id |> not_)
end
