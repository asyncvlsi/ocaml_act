open! Core
module Id = Int

module T = struct
  type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
  [@@deriving sexp, hash, equal, compare]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let bitwidth v = v.bitwidth
