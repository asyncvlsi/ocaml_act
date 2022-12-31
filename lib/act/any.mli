open! Core

type t [@@deriving sexp]

module A : sig
  type t [@@deriving sexp]
end

module B : sig
  type t [@@deriving sexp]
end

module C : sig
  type t [@@deriving sexp]
end
