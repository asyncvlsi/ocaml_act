open! Core
module Id = Int

module T = struct
  type t = { id : Id.t; bitwidth : int } [@@deriving sexp, hash, equal, compare]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let bitwidth v = v.bitwidth
