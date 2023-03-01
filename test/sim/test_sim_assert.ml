open! Core
open! Act

let%expect_test "assert true" =
  let ir = Chp.assert_ Expr.true_ in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}]

let%expect_test "assert false" =
  let ir = Chp.assert_ Expr.false_ in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}]
