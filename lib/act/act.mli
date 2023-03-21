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

val block11 :
  'i1 Chan.W.t ->
  'o1 Chan.R.t ->
  f:('i1 Chan.R.t -> 'o1 Chan.W.t -> Chp.t) ->
  Chp.t
