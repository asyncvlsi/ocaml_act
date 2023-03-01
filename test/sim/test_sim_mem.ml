open! Core
open! Act

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let var2 = Var.create CInt.dtype_32 in
  let ir =
    Chp.par
      [
        Chp.seq
          [
            Chp.read_ug_mem mem ~idx:CInt.E.(of_int 3) ~dst:var1;
            Chp.log1 var1 ~f:CInt.to_string;
          ];
        Chp.seq
          [
            Chp.read_ug_mem mem ~idx:CInt.E.(of_int 3) ~dst:var2;
            Chp.log1 var2 ~f:CInt.to_string;
          ];
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect
    {|
    (Error
     "Simulatnious accesses of a memory/rom: statement 1 in test/ir_test.ml on line 367, statement 2 in test/ir_test.ml on line 362.") |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.read_ug_mem mem ~idx:CInt.E.(of_int 4) ~dst:var1;
        Chp.log1 var1 ~f:CInt.to_string;
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect
    {|
    (Error
     "Mem access out of bounds: in test/ir_test.ml on line 339, idx is 4, size of mem is 4.") |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.read_ug_mem mem ~idx:CInt.E.(of_int 3) ~dst:var1;
        Chp.log1 var1 ~f:CInt.to_string;
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    4(Ok ()) |}]
