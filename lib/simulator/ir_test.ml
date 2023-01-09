open! Core
open! Act

let%expect_test "test1" =
  let ir =
    let var0 = Var.create DType.int_ in
    let var1 = Var.create DType.int_ in
    let chan2 = Chan.create DType.int_ in
    N.seq
      [
        N.assign var0 (Expr.const 12345);
        N.par
          [
            N.loop [ N.send chan2.w (Expr.var var0) ];
            N.seq
              [
                N.read chan2.r var1;
                N.log Expr.(var var1 |> map ~f:Int.to_string);
              ];
          ];
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    12345(Ok ()) |}]

let%expect_test "test2" =
  let ir =
    let var0 = Var.create DType.int_ ~init:123456 in
    let var1 = Var.create DType.int_ ~init:1 in
    N.seq
      [
        N.assign var0 (Expr.const 123456);
        N.assign var1 (Expr.const 1);
        N.while_loop
          Expr.(ne (var var0) (const 1))
          [
            N.assign var1 Expr.(var var1 |> add (const 1));
            N.if_else
              Expr.(mod_ (var var0) (const 2) |> eq (const 0))
              [ N.assign var0 Expr.(div (var var0) (const 2)) ]
              [
                N.assign var0 Expr.(var var0 |> mul (const 3) |> add (const 1));
              ];
          ];
        N.log Expr.(var var1 |> map ~f:(sprintf "%d\n"));
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    62
    (Ok ()) |}]

let%expect_test "test3" =
  let var = Var.create DType.int_ in
  let chan = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.log (Expr.const "start\n");
        N.read chan.r var;
        N.log (Expr.const "recv 1\n");
        N.read chan.r var;
        N.log (Expr.const "recv 2\n");
        N.read chan.r var;
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan.w.u ] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}];
  Sim.send sim chan.w 100;
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.send sim chan.w 200;
  Sim.wait' sim ();
  [%expect {|
    recv 2
    (Ok ()) |}];
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];
  Sim.send sim chan.w 300;
  Sim.wait' sim ();
  [%expect {|
    done
    (Ok ()) |}]

let%expect_test "test3" =
  let var1 = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.log (Expr.const "start\n");
        N.read chan1.r var1;
        N.log (Expr.const "recv 1\n");
        N.send chan2.w Expr.(var var1);
        N.log (Expr.const "send 1\n");
        N.assert_ Expr.(var var1 |> eq (const 200));
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}];
  Sim.send sim chan1.w 200;
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.read sim chan2.r 200;
  Sim.wait' sim ();
  [%expect {|
    send 1
    done
    (Ok ()) |}]

let%expect_test "test4" =
  let var1 = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.seq [];
        N.seq [];
        N.log (Expr.const "start\n");
        N.read chan1.r var1;
        N.seq [];
        N.log (Expr.const "recv 1\n");
        N.send chan2.w Expr.(var var1);
        N.log (Expr.const "send 1\n");
        N.assert_ Expr.(var var1 |> eq (const 210));
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}];
  Sim.send sim chan1.w 200;
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.read sim chan2.r 200;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
      send 1
      (Error "Assertion failed: in lib/simulator/ir_test.ml on line 147.") |}]

let%expect_test "test5" =
  let var1 = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir = N.loop [ N.read chan1.r var1; N.send chan2.w Expr.(var var1) ] in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];
  Sim.send sim chan1.w 1;
  Sim.send sim chan1.w 2;
  Sim.send sim chan1.w 3;
  Sim.send sim chan1.w 4;
  Sim.send sim chan1.w 5;

  Sim.read sim chan2.r 1;
  Sim.read sim chan2.r 2;
  Sim.read sim chan2.r 3;
  Sim.read sim chan2.r 5;
  Sim.read sim chan2.r 5;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
      (Error
       "User read has wrong value: got 4, but expected 5 based on `send' function call in lib/simulator/ir_test.ml on line 191, on chan created in lib/simulator/ir_test.ml on line 174.") |}]

let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create DType.bool_ ~init:false in
  N.loop
    [
      N.read i1 var1;
      N.if_else Expr.(var b1) [ N.send' o1 var1 ] [ N.send' o2 var1 ];
      N.toggle b1;
    ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create DType.bool_ ~init:false in
  N.loop
    [
      N.if_else Expr.(var b1) [ N.read i1 var1 ] [ N.read i2 var1 ];
      N.send o1 Expr.(var var1);
      N.toggle b1;
    ]

let rec buff ~depth ~dtype i1 o1 =
  if depth <= 0 then failwith "depth too low"
  else if Int.equal depth 1 then
    let chan1 = Chan.create dtype in
    let chan2 = Chan.create dtype in
    N.par [ split ~dtype i1 chan1.w chan2.w; merge ~dtype chan1.r chan2.r o1 ]
  else
    let chan1a = Chan.create dtype in
    let chan1b = Chan.create dtype in
    let chan2a = Chan.create dtype in
    let chan2b = Chan.create dtype in
    N.par
      [
        split ~dtype i1 chan1a.w chan2a.w;
        buff ~dtype ~depth:(depth - 1) chan1a.r chan1b.w;
        buff ~dtype ~depth:(depth - 1) chan2a.r chan2b.w;
        merge ~dtype chan1b.r chan2b.r o1;
      ]

let%expect_test "test_buff 1" =
  let dtype = DType.int_ in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:1 ~dtype i o) in
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in
  Sim.send sim i 7;

  Sim.read sim o 7;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ()) |}];

  Sim.send sim i 1;
  Sim.send sim i 2;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];

  Sim.read sim o 1;
  Sim.read sim o 2;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}];

  Sim.read sim o 8;
  Sim.read sim o 88;
  Sim.send sim i 8;
  Sim.send sim i 88;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ()) |}];

  Sim.send sim i 12;
  Sim.send sim i 13;
  Sim.send sim i 14;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
    (Error
     "User send did not complete:  called in lib/simulator/ir_test.ml on line 274, on chan created in lib/simulator/ir_test.ml on line 240.") |}]

let%expect_test "test_buff 2" =
  let dtype = DType.int_ in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in

  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.send sim i v);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.read sim o v);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}];

  List.iter [ 1; 2; 3; 4; 5; 6; 7 ] ~f:(fun v -> Sim.send sim i v);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
    (Error
     "User send did not complete:  called in lib/simulator/ir_test.ml on line 298, on chan created in lib/simulator/ir_test.ml on line 283.") |}]

let%expect_test "mem" =
  let mem = UnguardedMem.create DType.int_ [| 1; 2; 3; 4 |] in
  let var1 = Var.create DType.int_ in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:Expr.(const 3) ~dst:var1;
        N.log Expr.(var var1 |> map ~f:Int.to_string);
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  [%expect {|
    4(Ok ()) |}]

let%expect_test "mem" =
  let mem = UnguardedMem.create DType.int_ [| 1; 2; 3; 4 |] in
  let var1 = Var.create DType.int_ in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:Expr.(const 4) ~dst:var1;
        N.log Expr.(var var1 |> map ~f:Int.to_string);
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  (* TODO this test is wrong *)
  [%expect
    {|
    (Error
     "Mem access out of bounds: in lib/simulator/ir_test.ml on line 326, idx is 4, size of mem is 4.") |}]

let%expect_test "mem" =
  let mem = UnguardedMem.create DType.int_ [| 1; 2; 3; 4 |] in
  let var1 = Var.create DType.int_ in
  let ir =
    N.par
      [
        N.seq
          [
            N.read_ug_mem mem ~idx:Expr.(const 3) ~dst:var1;
            N.log Expr.(var var1 |> map ~f:Int.to_string);
          ];
        N.seq
          [
            N.read_ug_mem mem ~idx:Expr.(const 3) ~dst:var1;
            N.log Expr.(var var1 |> map ~f:Int.to_string);
          ];
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  (* This is of the dummy variable assoiated with the mem. This is misleading, and should report a better message *)
  [%expect
    {|
    (Error
     "Simulatnious writes of variable: statement 1 in lib/simulator/ir_test.ml on line 351, statement 2 in lib/simulator/ir_test.ml on line 346, create in lib/simulator/ir_test.ml on line 340.") |}]
