open! Core

module type S = sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable with type t := t
  include Hashable with type t := t
end
