open! Core

type t = Fixed of int | Unlimited [@@deriving sexp]

let max a b =
  match (a, b) with
  | Fixed a, Fixed b -> Fixed (Int.max a b)
  | Fixed _, Unlimited | Unlimited, Fixed _ | Unlimited, Unlimited -> Unlimited

let min a b =
  match (a, b) with
  | Fixed a, Fixed b -> Fixed (Int.min a b)
  | Fixed v, Unlimited | Unlimited, Fixed v -> Fixed v
  | Unlimited, Unlimited -> Unlimited

let one = Fixed 1

let ( + ) a b =
  match (a, b) with
  | Fixed a, Fixed b -> Fixed (a + b)
  | Fixed _, Unlimited | Unlimited, Fixed _ | Unlimited, Unlimited -> Unlimited
