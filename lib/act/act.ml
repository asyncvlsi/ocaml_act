open! Core

module Internal_rep = struct
  module Chan = Chan.Ir
  module DType = Dtype.Ir
  module Expr = Expr.Ir
  module N = Node.Ir
  module Mem = Mem.Ir
  module Var = Var.Ir
  module Layout = Layout
end

let block11 i1 o1 ~f = f (Chan.Ir.r_of_w i1) (Chan.Ir.w_of_r o1)

module Any = Any
module Chan = Chan.Wrap
module CInt = Cint
module CBool = Cbool
module Code_pos = Code_pos
module DType = Dtype.Wrap
module Expr = Expr.Wrap
module Mem = Mem.Wrap
module N = Node.Wrap
module Var = Var.Wrap
module Vec = Vec
