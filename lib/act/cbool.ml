open! Core
include Cbool0
include Comparable.Make (Cbool0)
include Hashable.Make (Cbool0)

let to_int t = to_cint t
let of_int i = of_cint i
let bitwidth t = to_int t |> Act_ir.CInt.bitwidth
let dtype = Ir_dtype.cbool_dtype |> Dtype.Internal.wrap

module E = struct
  type nonrec t = t Expr.t

  let var = Expr.var
  let const = Expr.of_bool
  let of_int = Expr.bool_of_int
  let to_int = Expr.int_of_bool
  let true_ = Expr.true_
  let false_ = Expr.false_
  let not_ = Expr.not_
  let eq = Expr.bool_eq
  let ne = Expr.bool_ne
  let and_ = Expr.and_
  let or_ = Expr.or_
  let xor_ = Expr.xor_
end

module Chp = struct
  type t = Chp.t

  let match_ expr ~f =
    Chp.select_imm ~else_:None
      (List.map [ false; true ] ~f:(fun op -> (E.(eq (const op) expr), f op)))

  let toggle (var_id : Cbool0.t Var.t) =
    Chp.assign var_id E.(var var_id |> not_)

  let set_true var_id = Chp.assign var_id E.true_
  let set_false var_id = Chp.assign var_id E.false_
end
