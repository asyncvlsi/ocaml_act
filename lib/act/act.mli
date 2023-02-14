open! Core

(* The internal data structures. These are only meant to be constructed throguh the above interfaces. *)
module Internal_rep : sig
  module Chan = Chan.Ir
  module DType = Dtype.Ir
  module Expr = Expr.Ir
  module Mem = Mem.Ir
  module Chp = Chp_node.Ir
  module Var = Var.Ir
  module Layout = Layout
end

module Any = Any
module Chan = Chan.Wrap
module CInt = Cint
module CBool = Cbool
module Code_pos = Code_pos
module DType = Dtype.Wrap
module Enum = Enum
module Expr = Expr.Wrap
module Mem = Mem.Wrap
module Chp = Chp_node.Wrap
module Var = Var.Wrap
module Vec = Vec

val block11 :
  'i1 Chan.W.t ->
  'o1 Chan.R.t ->
  f:('i1 Chan.R.t -> 'o1 Chan.W.t -> Chp.t) ->
  Chp.t
