open! Core
open! Act

let%expect_test "test log0" =
  let ir = Chp.log0 "hello world\n" in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    12345(Ok ()) |}]

let%expect_test "test log1" =
  let ir =
    let var0 = Var.create CInt.dtype_32 in
    Chp.seq
      [
        Chp.assign var0 CInt.E.(of_int 12345);
        Chp.log1 var1 ~f:(fun i -> [%string "%{i#CInt}\n"]);
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    12345(Ok ()) |}]

let%expect_test "test repeated log" =
  let ir =
    let var0 = Var.create CInt.dtype_32 ~init:CInt.zero in
    Chp.loop_while
      Expr.(lt var0 five)
      [
        CInt.Chp.assign var0 Expr.(var var0 |> add one) ~overflow:`Cant;
        Chp.log1 var1 ~f:(fun i -> [%string "%{i#CInt}\n"]);
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    12345(Ok ()) |}]
