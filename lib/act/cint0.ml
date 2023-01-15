open! Core

module T = struct
  type t = Bigint.t [@@deriving hash, compare, equal]

  let sexp_of_t t = Bigint.sexp_of_t t

  let t_of_sexp sexp =
    let i = Bigint.t_of_sexp sexp in
    assert (Bigint.(i >= zero));
    i
end

include T

let of_string s = Sexp.of_string s |> t_of_sexp
let to_string t = Sexp.to_string (sexp_of_t t)

include Comparable.Make (T)
include Hashable.Make (T)

let of_int (i : int) =
  assert (Int.(i >= 0));
  Bigint.of_int i

let bitwidth t =
  if Bigint.equal t (of_int 0) then 1
  else Bigint.to_zarith_bigint t |> Z.numbits

let ( + ) a b = Bigint.(a + b)

let ( - ) a b =
  let r = Bigint.(a - b) in
  assert (Bigint.(r >= zero));
  r

let pow a b = Bigint.(pow a b)

let%expect_test "width" =
  let f i = print_s [%sexp ((i, bitwidth (of_int i)) : int * int)] in
  f 0;
  f 1;
  f 2;
  f 3;
  f 7;
  f 8;
  f 19;
  f 260;
  [%expect
    {|
    (0 1)
    (1 1)
    (2 2)
    (3 2)
    (7 3)
    (8 4)
    (19 5)
    (260 9) |}]
