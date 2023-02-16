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

let to_int_exn t = Bigint.to_int_exn t

let of_int (i : int) =
  assert (Int.(i >= 0));
  Bigint.of_int i

let bitwidth t =
  if Bigint.equal t (of_int 0) then 1
  else Bigint.to_zarith_bigint t |> Z.numbits

let add a b = Bigint.(a + b)

let sub a b =
  let r = Bigint.(a - b) in
  assert (Bigint.(r >= zero));
  r

let mul a b = Bigint.(a * b)
let div a b = Bigint.(a / b)
let mod_ a b = Bigint.(a % b)
let zero = of_int 0
let one = of_int 1
let two = of_int 2
let three = of_int 3
let four = of_int 4
let five = of_int 5
let left_shift a ~amt:b = Bigint.shift_left a (Bigint.to_int_exn b)
let right_shift a ~amt:b = Bigint.( / ) a (Bigint.pow two b)
let bit_and a b = Bigint.bit_and a b
let bit_or a b = Bigint.bit_or a b
let bit_xor a b = Bigint.bit_xor a b
let pow a b = Bigint.pow a b
let eq a b = Bigint.equal a b
let ne a b = not (eq a b)
let lt a b = Bigint.(a < b)
let le a b = Bigint.(a <= b)
let gt a b = Bigint.(a > b)
let ge a b = Bigint.(a >= b)
let clip t ~bits = bit_and t (sub (pow two (of_int bits)) one)
let add_wrap a b ~bits = add a b |> clip ~bits

let sub_wrap a b ~bits =
  (* TODO this could be better *)
  let shift = mul b (pow two (of_int bits)) in
  sub (add shift a) b |> clip ~bits

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
