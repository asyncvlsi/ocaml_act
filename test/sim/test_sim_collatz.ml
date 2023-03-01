open! Core
open! Act

let%expect_test "test2" =
  let ir =
    let var0 = Var.create CInt.dtype_32 ~init:(CInt.of_int 123456) in
    let var1 = Var.create CInt.dtype_32 ~init:(CInt.of_int 1) in
    Chp.seq
      [
        Chp.while_loop
          CInt.E.(ne (var var0) (of_int 1))
          [
            CInt.Chp.assign var1
              CInt.E.(var var1 |> add (of_int 1))
              ~overflow:Cant;
            Chp.if_else
              CInt.E.(mod_ (var var0) (of_int 2) |> eq (of_int 0))
              [ Chp.assign var0 CInt.E.(div (var var0) (of_int 2)) ]
              [
                CInt.Chp.assign var0
                  CInt.E.(var var0 |> mul (of_int 3) |> add (of_int 1))
                  ~overflow:Cant;
              ];
          ];
        Chp.log1 var1 ~f:(fun v -> [%string "%{v#CInt}\n"]);
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    62
    (Ok ()) |}]

let%expect_test "test2" =
  let ir dtype init_val =
    let var0 = Var.create dtype ~init:(CInt.of_int init_val) in
    let var1 = Var.create dtype ~init:(CInt.of_int 1) in
    Chp.seq
      [
        Chp.while_loop
          CInt.E.(ne (var var0) (of_int 1))
          [
            CInt.Chp.assign var1
              CInt.E.(var var1 |> add (of_int 1))
              ~overflow:Cant;
            Chp.if_else
              CInt.E.(mod_ (var var0) two |> zero)
              [ Chp.assign var0 Expr.(shift_right' (var var0) ~amt:1) ]
              [
                CInt.Chp.assign var0
                  CInt.E.(var var0 |> mul (of_int 3) |> add (of_int 1))
                  ~overflow:Cant;
              ];
          ];
        Chp.log1 var1 ~f:(fun v -> [%string "%{v#CInt}\n"]);
      ]
  in
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      let ir = ir (CInt.dtype ~bits:14) 123456 in
      let sim =
        Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
      in
      print_s [%sexp (Sim.wait sim () : unit Or_error.t)]);
  [%expect
    {|
    (Failure
     "Trying to initialize a variable of dtype (Bits_fixed 14) with a value of max_layout (Bits_fixed 17).") |}];
  let ir = ir (CInt.dtype ~bits:6) 63 in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect
    {|
    (Error
     "Assigned value doesnt fit in var: got 190 but variable has layout (Bits_fixed 6) at in test/ir_test.ml on line 536.") |}]
