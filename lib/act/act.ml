open! Core
module Chan = Chan
module Chp = Chp
module CInt = Cint
module CBool = Cbool
module DType = Dtype
module CEnum = C_enum
module Expr = Expr
module Mem = Mem
module Overflow_behavior = Overflow_behavior
module Process = Process
module Var = Var

let branch ~(guard : CBool.t Expr.t) (stmts : Chp.t list) = (guard, Chp.seq stmts)

(** libraries for processing Act source code *)

module Compiler = Compiler
module Sim = Sim
