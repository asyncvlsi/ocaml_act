open! Core
open! Act

let%expect_test "test1" =
  let ir =
    let var0 = Var.create DType.int32 in
    let var1 = Var.create DType.int32 in
    let chan2 = Chan.create DType.int32 in
    N.seq
      [
        N.assign var0 Expr.CInt_.(cint 12345);
        N.par
          [
            N.loop [ N.send chan2.w Expr.CInt_.(var var0) ];
            N.seq
              [
                N.read chan2.r var1;
                N.log Expr.(var var1 |> map ~f:CInt.to_string);
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
    let var0 = Var.create DType.int32 ~init:(CInt.of_int 123456) in
    let var1 = Var.create DType.int32 ~init:(CInt.of_int 1) in
    N.seq
      [
        N.while_loop
          Expr.CInt_.(ne (var var0) (cint 1))
          [
            N.CInt_.assign var1
              Expr.CInt_.(var var1 |> add (cint 1))
              ~overflow:Cant;
            N.if_else
              Expr.CInt_.(mod_ (var var0) (cint 2) |> eq (cint 0))
              [ N.assign var0 Expr.CInt_.(div (var var0) (cint 2)) ]
              [
                N.CInt_.assign var0
                  Expr.CInt_.(var var0 |> mul (cint 3) |> add (cint 1))
                  ~overflow:Cant;
              ];
          ];
        N.log Expr.(var var1 |> map ~f:(fun v -> [%string "%{v#CInt}\n"]));
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    62
    (Ok ()) |}]

let%expect_test "test3" =
  let dtype = DType.int_ ~bits:9 in
  let var = Var.create dtype in
  let chan = Chan.create dtype in
  let ir =
    N.seq
      [
        N.log (Expr.String_.const "start\n");
        N.read chan.r var;
        N.log (Expr.String_.const "recv 1\n");
        N.read chan.r var;
        N.log (Expr.String_.const "recv 2\n");
        N.read chan.r var;
        N.log (Expr.String_.const "done\n");
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan.w.u ] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    start
    (Ok ()) |}];
  Sim.send sim chan.w (CInt.of_int 100);
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.send sim chan.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    recv 2
    (Ok ()) |}];
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}];
  Sim.send sim chan.w (CInt.of_int 300);
  Sim.wait' sim ();
  [%expect {|
    done
    (Ok ()) |}]

let%expect_test "test3" =
  let var1 = Var.create DType.int32 in
  let chan1 = Chan.create DType.int32 in
  let chan2 = Chan.create DType.int32 in
  let ir =
    N.seq
      [
        N.log (Expr.String_.const "start\n");
        N.read chan1.r var1;
        N.log (Expr.String_.const "recv 1\n");
        N.send chan2.w Expr.(var var1);
        N.log (Expr.String_.const "send 1\n");
        N.assert_ Expr.CInt_.(var var1 |> eq (cint 200));
        N.log (Expr.String_.const "done\n");
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
  Sim.send sim chan1.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.read sim chan2.r (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    send 1
    done
    (Ok ()) |}]

let%expect_test "test4" =
  let var1 = Var.create DType.int32 in
  let chan1 = Chan.create DType.int32 in
  let chan2 = Chan.create DType.int32 in
  let ir =
    N.seq
      [
        N.seq [];
        N.seq [];
        N.log (Expr.String_.const "start\n");
        N.read chan1.r var1;
        N.seq [];
        N.log (Expr.String_.const "recv 1\n");
        N.send chan2.w Expr.(var var1);
        N.log (Expr.String_.const "send 1\n");
        N.assert_ Expr.CInt_.(var var1 |> eq (cint 210));
        N.log (Expr.String_.const "done\n");
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
  Sim.send sim chan1.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    recv 1
    (Ok ()) |}];
  Sim.read sim chan2.r (CInt.of_int 200);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
      send 1
      (Error "Assertion failed: in lib/simulator/ir_test.ml on line 150.") |}]

let%expect_test "test5" =
  let var1 = Var.create DType.int32 in
  let chan1 = Chan.create DType.int32 in
  let chan2 = Chan.create DType.int32 in
  let ir = N.loop [ N.read chan1.r var1; N.send chan2.w Expr.(var var1) ] in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan1.w.u ]
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
       "User read has wrong value: got 4, but expected 5 based on `send' function call in lib/simulator/ir_test.ml on line 194, on chan created in lib/simulator/ir_test.ml on line 177.") |}]

let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create DType.bool_ ~init:CBool.false_ in
  N.loop
    [
      N.read i1 var1;
      N.if_else Expr.(var b1) [ N.send' o1 var1 ] [ N.send' o2 var1 ];
      N.toggle b1;
    ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create DType.bool_ ~init:CBool.false_ in
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
  let dtype = DType.int32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:1 ~dtype i o) in
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in
  Sim.send sim i (CInt.of_int 7);

  Sim.read sim o (CInt.of_int 7);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ()) |}];

  Sim.send sim i (CInt.of_int 1);
  Sim.send sim i (CInt.of_int 2);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];

  Sim.read sim o (CInt.of_int 1);
  Sim.read sim o (CInt.of_int 2);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}];

  Sim.read sim o (CInt.of_int 8);
  Sim.read sim o (CInt.of_int 88);
  Sim.send sim i (CInt.of_int 8);
  Sim.send sim i (CInt.of_int 88);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ()) |}];

  Sim.send sim i (CInt.of_int 12);
  Sim.send sim i (CInt.of_int 13);
  Sim.send sim i (CInt.of_int 14);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
    (Error
     "User send did not complete:  called in lib/simulator/ir_test.ml on line 277, on chan created in lib/simulator/ir_test.ml on line 243.") |}]

let%expect_test "test_buff 2" =
  let dtype = DType.int32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in

  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.send sim i (CInt.of_int v));
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.read sim o (CInt.of_int v));
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}];

  List.iter [ 1; 2; 3; 4; 5; 6; 7 ] ~f:(fun v -> Sim.send sim i (CInt.of_int v));
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
    (Error
     "User send did not complete:  called in lib/simulator/ir_test.ml on line 301, on chan created in lib/simulator/ir_test.ml on line 286.") |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem DType.int32 arr
  in
  let var1 = Var.create DType.int32 in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:Expr.CInt_.(cint 3) ~dst:var1;
        N.log Expr.(var var1 |> map ~f:CInt.to_string);
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  [%expect {|
    4(Ok ()) |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem DType.int32 arr
  in
  let var1 = Var.create DType.int32 in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:Expr.CInt_.(cint 4) ~dst:var1;
        N.log Expr.(var var1 |> map ~f:CInt.to_string);
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  (* TODO this test is wrong *)
  [%expect
    {|
    (Error
     "Mem access out of bounds: in lib/simulator/ir_test.ml on line 335, idx is 4, size of mem is 4.") |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem DType.int32 arr
  in
  let var1 = Var.create DType.int32 in
  let var2 = Var.create DType.int32 in
  let ir =
    N.par
      [
        N.seq
          [
            N.read_ug_mem mem ~idx:Expr.CInt_.(cint 3) ~dst:var1;
            N.log Expr.(var var1 |> map ~f:CInt.to_string);
          ];
        N.seq
          [
            N.read_ug_mem mem ~idx:Expr.CInt_.(cint 3) ~dst:var2;
            N.log Expr.(var var2 |> map ~f:CInt.to_string);
          ];
      ]
  in
  let sim = Sim.create ir ~user_sendable_ports:[] ~user_readable_ports:[] in
  Sim.wait' sim ();
  [%expect
    {|
    (Error
     "Simulatnious accesses of a memory/rom: statement 1 in lib/simulator/ir_test.ml on line 364, statement 2 in lib/simulator/ir_test.ml on line 359.") |}]

let%expect_test "test probes" =
  let var = Var.create DType.int32 in
  let chan = Chan.create DType.int32 in
  let ir =
    N.par
      [
        N.seq
          [
            N.log (Expr.String_.const "A ");
            N.wait_probe_w chan.w;
            N.log (Expr.String_.const "B ");
            N.log (Expr.String_.const "C ");
            N.send chan.w Expr.CInt_.(cint 3);
            N.log (Expr.String_.const "D ");
            N.log (Expr.String_.const "E ");
            N.log (Expr.String_.const "F ");
          ];
        N.seq
          [
            N.log (Expr.String_.const "1 ");
            N.log (Expr.String_.const "2 ");
            N.log (Expr.String_.const "3 ");
            N.log (Expr.String_.const "4 ");
            N.read chan.r var;
            N.log (Expr.String_.const "5 ");
          ];
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:[ chan.w.u ] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    A 1 2 3 4 B C D E 5 F (Ok ()) |}]
