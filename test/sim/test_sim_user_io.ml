open! Core
open! Act

let%expect_test "one send" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir = Chp.seq [ Chp.read chan.r var; Chp.log "did send\n" ] in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan.w.u ]
      ~user_readable_ports:[]
  in

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.send sim chan.w (CInt.of_int 100);
  Sim.wait' sim ();
  [%expect {|
    did read
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}]

let%expect_test "many sends" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.read chan.r var;
        Chp.log "did send 1\n";
        Chp.read chan.r var;
        Chp.log "did send 2\n";
        Chp.read chan.r var;
        Chp.log "did send 3\n";
        Chp.read chan.r var;
        Chp.log "did send 4\n";
        Chp.read chan.r var;
        Chp.log "did send 5\n";
        Chp.read chan.r var;
        Chp.log "did send 6\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan.w.u ]
      ~user_readable_ports:[]
  in

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.send sim chan.w (CInt.of_int 100);
  Sim.wait' sim ();
  [%expect {|
    did send 1
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.send sim chan.w (CInt.of_int 100);
  Sim.send sim chan.w (CInt.of_int 200);
  Sim.send sim chan.w (CInt.of_int 300);
  Sim.wait' sim ();
  [%expect {|
    did send 2
    did send 3
    did send 4
    (Ok ()) |}];

  Sim.send sim chan.w (CInt.of_int 100);
  Sim.send sim chan.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    did send 5
    did send 6
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}]

let%expect_test "one read" =
  let var = Var.create CInt.dtype_32 ~init:(CInt.of_int 123) in
  let chan = Chan.create CInt.dtype_32 in
  let ir = Chp.seq [ Chp.send chan.w (Expr.var var); Chp.log "did read\n" ] in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[]
      ~user_readable_ports:[ chan.r.u ]
  in

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.read sim chan.r (CInt.of_int 123);
  Sim.wait' sim ();
  [%expect {|
    did read
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}]

let%expect_test "many reads" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.send_var chan.r Expr.zero;
        Chp.log "did read 1\n";
        Chp.send_var chan.w Expr.one;
        Chp.log "did read 2\n";
        Chp.send_var chan.w Expr.two;
        Chp.log "did read 3\n";
        Chp.send_var chan.w Expr.three;
        Chp.log "did read 4\n";
        Chp.send_var chan.w Expr.four;
        Chp.log "did read 5\n";
        Chp.send_var chan.w Expr.five;
        Chp.log "did read 6\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[]
      ~user_readable_ports:[ chan.r.u ]
  in

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.read sim chan.r CInt.zero;
  Sim.wait' sim ();
  [%expect {|
    did read 1
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.read sim chan.w CInt.one;
  Sim.read sim chan.w CInt.two;
  Sim.read sim chan.w CInt.three;
  Sim.wait' sim ();
  [%expect {|
    did read 2
    did read 3
    did read 4
    (Ok ()) |}];

  Sim.read sim chan.w CInt.four;
  Sim.read sim chan.w CInt.five;
  Sim.wait' sim ();
  [%expect {|
    did read 5
    did read 6
    (Ok ()) |}];

  Sim.wait' sim ();
  [%expect {| (Ok ()) |}]

let%expect_test "mixed sends and reads" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.read chan1.r var1;
        Chp.send chan2.w Expr.(var var1);
        Chp.send chan2.w Expr.(var var1 |> add three);
        Chp.log "done\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];

  Sim.read sim chan2.r (CInt.of_int 200);
  Sim.read sim chan2.r (CInt.of_int 203);
  Sim.read sim chan1.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    done
    (Ok ()) |}]

let%expect_test "error send too big value" =
  let var1 = Var.create CInt.dtype_8 in
  let chan1 = Chan.create CInt.dtype_8 in
  let ir = Chp.read chan1.r var1 in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[]
  in

  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      Sim.send sim chan1.w (CInt.of_int 1000));
  [%expect
    {|
    (Failure
     "Value doesnt fit in chan: got value 1000 but channel has layout (Bits_fixed 8).") |}]

let%expect_test "error read too big value" =
  let var1 = Var.create CInt.dtype_8 ~init:CInt.zero in
  let chan1 = Chan.create CInt.dtype_8 in
  let ir = Chp.send_var chan1.r var1 in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[]
      ~user_readable_ports:[ chan1.r.u ]
  in

  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      Sim.read sim chan1.r (CInt.of_int 1000));
  [%expect
    {|
    (Failure
     "Value doesnt fit in chan: got value 1000 but channel has layout (Bits_fixed 8).") |}]

let%expect_test "error read too incorrect value" =
  let var1 = Var.create CInt.dtype_8 ~init:CInt.three in
  let chan1 = Chan.create CInt.dtype_8 in
  let ir = Chp.send_var chan1.r var1 in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[]
      ~user_readable_ports:[ chan1.r.u ]
  in

  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      Sim.read sim chan1.r (CInt.of_int 5));
  [%expect
    {|
    (Failure
     "Value doesnt fit in chan: got value 1000 but channel has layout (Bits_fixed 8).") |}]
