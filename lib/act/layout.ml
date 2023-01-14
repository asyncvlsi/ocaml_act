open! Core

type t = Fixed of int | Unknown [@@deriving sexp_of]
