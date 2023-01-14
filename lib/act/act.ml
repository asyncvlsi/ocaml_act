open! Core
module Any = Any
module Chan = Chan
module CInt = Cint
module CBool = Cbool
module Code_pos = Code_pos
module DType = Dtype
module Expr = Expr
module Mem = Mem
module N = Node
module Var = Var
module Vec = Vec

let block11 i1 o1 ~f = f (Chan.Ir.r_of_w i1) (Chan.Ir.w_of_r o1)

module Internal_rep = struct
  module Chan = Chan.Ir
  module DType = Dtype.Ir
  module Expr = Expr.Ir
  module N = Node.Ir
  module Mem = Mem.Ir
  module Var = Var.Ir
end
