open! Core
open! Act

let%expect_test "test5" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.loop [ Chp.read chan1.r var1; Chp.send chan2.w Expr.(var var1) ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];
  Sim.send sim chan1.w (CInt.of_int 1);
  Sim.send sim chan1.w (CInt.of_int 2);
  Sim.send sim chan1.w (CInt.of_int 3);
  Sim.send sim chan1.w (CInt.of_int 4);
  Sim.send sim chan1.w (CInt.of_int 5);

  Sim.read sim chan2.r (CInt.of_int 1);
  Sim.read sim chan2.r (CInt.of_int 2);
  Sim.read sim chan2.r (CInt.of_int 3);
  Sim.read sim chan2.r (CInt.of_int 5);
  Sim.read sim chan2.r (CInt.of_int 5);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
      (Error
       "User read has wrong value: got 4, but expected 5 based on `send' function call in test/ir_test.ml on line 195, on chan created in test/ir_test.ml on line 176.") |}]
