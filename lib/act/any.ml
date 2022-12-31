type t [@@deriving sexp]

module A = struct
  type t [@@deriving sexp]
end

module B = struct
  type t [@@deriving sexp]
end

module C = struct
  type t [@@deriving sexp]
end
