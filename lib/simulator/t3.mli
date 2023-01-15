open! Core
open! Act

type t = A | B | C [@@deriving sexp]

val dtype : t DType.t
