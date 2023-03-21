open! Core

type 'a t = 'a Ir_dtype.t [@@deriving sexp_of]

module Internal = struct
  let unwrap t = t
  let wrap t = t
end
