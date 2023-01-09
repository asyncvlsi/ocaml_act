open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }

let create ~(equal : 'a -> 'a -> bool) ~(sexp_of_t : 'a -> Sexp.t) : 'a t =
  { equal = Obj.magic equal; sexp_of_t = Obj.magic sexp_of_t }

let of_module (type a) (module M : DTypeable with type t = a) : a t =
  create ~equal:M.equal ~sexp_of_t:M.sexp_of_t

let int_ = of_module (module Int)
let bool_ = of_module (module Bool)
let string_ = of_module (module String)
let equal_ (t : 'a t) (a : 'a) (b : 'a) = t.equal (Obj.magic a) (Obj.magic b)
let sexp_of_t_ (t : 'a t) (a : 'a) = t.sexp_of_t (Obj.magic a)
let equal_fn (t : 'a t) = Staged.stage (Obj.magic t.equal : 'a -> 'a -> bool)

let sexp_of_t_fn (t : 'a t) =
  Staged.stage (Obj.magic t.sexp_of_t : 'a -> Sexp.t)
