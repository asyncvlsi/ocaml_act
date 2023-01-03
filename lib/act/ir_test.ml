open! Core
open Ir

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
            N.loop (N.seq [ N.send chan2 (Var var0) ]);
            N.seq
              [
                N.read chan2 var1; N.log (Var var1 |> Expr.map ~f:Int.to_string);
              ];
          ];
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:Chan.U.Set.empty
      ~user_readable_ports:Chan.U.Set.empty
  in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {|
    12345Stuck |}]

let%expect_test "test2" =
  let ir =
    let var0 = Var.create DType.int_ in
    let var1 = Var.create DType.int_ in
    N.seq
      [
        N.assign var0 (Expr.const 123456);
        N.assign var1 (Expr.const 1);
        N.while_loop
          (Expr.map (Var var0) ~f:(fun v -> v > 1))
          (N.seq
             [
               N.assign var1 (Expr.map (Var var1) ~f:(fun v -> v + 1));
               N.if_else
                 (Expr.map (Var var0) ~f:(fun v -> v % 2 < 1))
                 (N.assign var0 (Expr.map (Var var0) ~f:(fun v -> v / 2)))
                 (N.assign var0 (Expr.map (Var var0) ~f:(fun v -> (v * 3) + 1)));
             ]);
        N.log (Var var1 |> Expr.map ~f:(fun v -> sprintf "%d\n" v));
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:Chan.U.Set.empty
      ~user_readable_ports:Chan.U.Set.empty
  in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {|
    62
    Stuck |}]

let%expect_test "test3" =
  let var = Var.create DType.int_ in
  let chan = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.log (Expr.const "start\n");
        N.read chan var;
        N.log (Expr.const "recv 1\n");
        N.read chan var;
        N.log (Expr.const "recv 2\n");
        N.read chan var;
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan.U.Set.of_list [ chan.u ])
      ~user_readable_ports:Chan.U.Set.empty
  in
  Sim.wait' sim;
  [%expect {|
    start |}];
  Sim.send sim chan (Obj.magic 100);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.send sim chan (Obj.magic 200);
  Sim.wait' sim;
  [%expect {|
    recv 2 |}];
  Sim.wait' sim;
  [%expect {||}];
  Sim.send sim chan (Obj.magic 300);
  Sim.wait' sim;
  [%expect {| done |}]

let%expect_test "test3" =
  let var = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.log (Expr.const "start\n");
        N.read chan1 var;
        N.log (Expr.const "recv 1\n");
        N.send chan2 (Var var);
        N.log (Expr.const "send 1\n");
        N.assert_ (Expr.map (Var var) ~f:(fun v -> Int.equal v 200));
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan.U.Set.of_list [ chan1.u ])
      ~user_readable_ports:(Chan.U.Set.of_list [ chan2.u ])
  in
  Sim.wait' sim;
  [%expect {|
    start |}];
  Sim.send sim chan1 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.read sim chan2 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {|
    send 1
    done |}]

let%expect_test "test4" =
  let var = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir =
    N.seq
      [
        N.seq [];
        N.seq [];
        N.log (Expr.const "start\n");
        N.read chan1 var;
        N.seq [];
        N.log (Expr.const "recv 1\n");
        N.send chan2 (Var var);
        N.log (Expr.const "send 1\n");
        N.assert_ (Expr.map (Var var) ~f:(fun v -> Int.equal v 210));
        N.log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan.U.Set.of_list [ chan1.u ])
      ~user_readable_ports:(Chan.U.Set.of_list [ chan2.u ])
  in
  Sim.wait' sim;
  [%expect {|
    start |}];
  Sim.send sim chan1 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.read sim chan2 (Obj.magic 200);
  let wait_outcome = Sim.wait sim in
  print_s [%sexp (wait_outcome : Sim.Wait_outcome.t)];
  [%expect
    {|
      send 1
      (Assert_failure
       ((filename lib/act/ir_test.ml) (line_number 146) (start_char 8)
        (end_char 68))) |}]

let%expect_test "test5" =
  let var = Var.create DType.int_ in
  let chan1 = Chan.create DType.int_ in
  let chan2 = Chan.create DType.int_ in
  let ir = N.loop (N.seq [ N.read chan1 var; N.send chan2 (Var var) ]) in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan.U.Set.of_list [ chan1.u ])
      ~user_readable_ports:(Chan.U.Set.of_list [ chan2.u ])
  in
  Sim.wait' sim;
  [%expect {| |}];
  Sim.send sim chan1 (Obj.magic 1);
  Sim.send sim chan1 (Obj.magic 2);
  Sim.send sim chan1 (Obj.magic 3);
  Sim.send sim chan1 (Obj.magic 4);
  Sim.send sim chan1 (Obj.magic 5);

  Sim.read sim chan2 (Obj.magic 1);
  Sim.read sim chan2 (Obj.magic 2);
  Sim.read sim chan2 (Obj.magic 3);
  Sim.read sim chan2 (Obj.magic 5);
  Sim.read sim chan2 (Obj.magic 5);
  let wait_outcome = Sim.wait sim in
  print_s [%sexp (wait_outcome : Sim.Wait_outcome.t)];
  [%expect
    {|
      (Read_dequeuer_wrong_value
       ((id 7) (dtype <opaque>) (creation_code_pos <opaque>)) 4 5) |}]
