open! Core

(* The internal data structures. These are only meant to be constructed throguh
   the above interfaces. *)
module Internal_rep : sig
  module Chan = Chan.Ir
  module Chp = Chp.Ir
  module DType = Dtype.Ir
  module Expr = Expr.Ir
  module Mem = Mem.Ir
  module Process = Process.Ir
  module Var = Var.Ir
  module Layout = Layout
end

module Any = Any
module Chan = Chan
module Chp = Chp
module CInt = Cint
module CBool = Cbool
module Code_pos = Code_pos
module DType = Dtype
module CEnum = C_enum
module Expr = Expr
module Mem = Mem
module Process = Process
module Var = Var
module Vec = Vec

val block11 :
  'i1 Chan.W.t ->
  'o1 Chan.R.t ->
  f:('i1 Chan.R.t -> 'o1 Chan.W.t -> Chp.t) ->
  Chp.t
