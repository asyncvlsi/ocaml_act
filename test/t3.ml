open! Core
open! Ochp
open! Ochp.Act

module T = struct
  type t = A | B | C [@@deriving sexp, equal, hash, compare]

  let mapping = [ (A, CInt.of_int 0); (B, CInt.of_int 1); (C, CInt.of_int 2) ]
end

include T
include CEnum.Make (T)
