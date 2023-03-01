open! Core
open! Act

let sim_true ir =
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ()

let sim_false here ir =
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Expect_test_helpers_core.require_does_raise here (fun () -> Sim.wait' sim ())

let%expect_test "equal correct 1" =
  let ir = Chp.assert_ Expr.(eq one one) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "equal correct 2" =
  let ir = Chp.assert_ Expr.(of_cint CInt.one |> eq (of_int 1)) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "equal incorrect 1" =
  let ir = Chp.assert_ Expr.(eq one four) in
  sim_false ir;
  [%expect {|
    start
    (Ok ()) |}]

let%expect_test "equal incorrect 2" =
  let cint = CInt.(pow two (of_int 128) |> add one) in
  let ir = Chp.assert_ Expr.(eq one (of_cint cint)) in
  sim_false ir;
  [%expect {|
    start
    (Ok ()) |}]

let%expect_test "add 1" =
  let ir = Chp.assert_ Expr.(add one two |> eq three) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "add 2" =
  let ir = Chp.assert_ Expr.(add (of_int 12) (of_int 27) |> eq (of_int 39)) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_no_underflow 1" =
  let ir = Chp.assert_ Expr.(sub_no_underflow five three |> eq two) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_no_underflow 2" =
  let ir =
    Chp.assert_
      Expr.(
        sub_no_underflow (of_int 123456789) (of_int 12345678)
        |> eq (of_int 111111111))
  in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_no_underflow 3 - underflows" =
  let ir =
    Chp.assert_ Expr.(sub_no_underflow (of_int 111) (of_int 123) |> eq zero)
  in
  sim_false ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_clip 1" =
  let ir = Chp.assert_ Expr.(sub_clip five three ~bits:8 |> eq two) in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_clip 2" =
  let ir =
    Chp.assert_
      Expr.(
        sub_clip (of_int 123456789) (of_int 12345678) ~bits:3 |> eq (of_int 7))
  in
  sim_true ir;
  [%expect {| (Ok ()) |}]

let%expect_test "sub_clip 3" =
  let ir =
    Chp.assert_
      Expr.(sub_clip (of_int 111) (of_int 123) ~bits:128 |> eq (of_int 244))
  in
  sim_true ir;
  [%expect {| (Ok ()) |}]
