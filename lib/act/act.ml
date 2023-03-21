open! Core
module Chan = Chan
module Chp = Chp
module CInt = Cint
module CBool = Cbool
module DType = Dtype
module CEnum = C_enum
module Expr = Expr
module Mem = Mem
module Process = Process
module Var = Var

let block11 i1 o1 ~f = f (Chan.Internal.r_of_w i1) (Chan.Internal.w_of_r o1)
