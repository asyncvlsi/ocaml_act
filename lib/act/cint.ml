open! Core
include Int

let of_int i = i
let width t = match t with 0 -> 1 | _ -> Int.num_bits - Int.clz t
let width' t = Width.Fixed (width t)

let%expect_test "width" =
  let f i = print_s [%sexp ((i, width' (of_int i)) : int * Width.t)] in
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
    (0 (Fixed 1))
    (1 (Fixed 1))
    (2 (Fixed 2))
    (3 (Fixed 2))
    (7 (Fixed 3))
    (8 (Fixed 4))
    (19 (Fixed 5))
    (260 (Fixed 9)) |}]
