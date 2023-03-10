open! Core
open! Act

let ir1 = Chp.assert_ Expr.true_

let%expect_test "assert true" =
  let sim =
    Sim.simulate_chp ir1 ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}]

let ir2 = Chp.assert_ Expr.false_

let%expect_test "assert false" =
  let sim =
    Sim.simulate_chp ir2 ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}]
