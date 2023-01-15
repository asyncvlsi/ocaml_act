open! Core
include Int

let of_int i = i
let bitwidth t = match t with 0 -> 1 | _ -> Int.num_bits - Int.clz t

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
