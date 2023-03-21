open! Core
open! Ochp
open! Ochp.Act

(* TODO switch stuff 4.14.0 opam install bignum core dune
   expect_test_helpers_core ocamlfmt *)

let%expect_test "test1" =
  let ir =
    let var0 = Var.create CInt.dtype_32 in
    let var1 = Var.create CInt.dtype_32 in
    let chan2 = Chan.create CInt.dtype_32 in
    Chp.seq
      [
        Chp.assign var0 CInt.E.(of_int 12345);
        Chp.par
          [
            Chp.loop [ Chp.send chan2.w CInt.E.(var var0) ];
            Chp.seq [ Chp.read chan2.r var1; Chp.log1 var1 ~f:CInt.to_string ];
          ];
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    12345(Ok ()) |}]

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

let%expect_test "test3" =
  let dtype = CInt.dtype ~bits:9 in
  let var = Var.create dtype in
  let chan = Chan.create dtype in
  let ir =
    Chp.seq
      [
        Chp.log "start\n";
        Chp.read chan.r var;
        Chp.log "recv 1\n";
        Chp.read chan.r var;
        Chp.log "recv 2\n";
        Chp.read chan.r var;
        Chp.log "done\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan.w.u ]
      ~user_readable_ports:[]
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
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.log "start\n";
        Chp.read chan1.r var1;
        Chp.log "recv 1\n";
        Chp.send chan2.w Expr.(var var1);
        Chp.log "send 1\n";
        Chp.assert_ CInt.E.(var var1 |> eq (of_int 200));
        Chp.log "done\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan1.w.u ]
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
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.seq [];
        Chp.seq [];
        Chp.log "start\n";
        Chp.read chan1.r var1;
        Chp.seq [];
        Chp.log "recv 1\n";
        Chp.send chan2.w Expr.(var var1);
        Chp.log "send 1\n";
        Chp.assert_ CInt.E.(var var1 |> eq (of_int 210));
        Chp.log "done\n";
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan1.w.u ]
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
    (Error "Assertion failed: in test/ir_test.ml on line 155.") |}]

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
       "User read has wrong value: got 4, but expected 5 based on `send' function call in test/ir_test.ml on line 201, on chan created in test/ir_test.ml on line 182.") |}]

let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.read i1 var1;
      Chp.if_else
        Expr.(var b1)
        [ Chp.send_var o1 var1 ]
        [ Chp.send_var o2 var1 ];
      CBool.Chp.toggle b1;
    ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.if_else Expr.(var b1) [ Chp.read i1 var1 ] [ Chp.read i2 var1 ];
      Chp.send o1 Expr.(var var1);
      CBool.Chp.toggle b1;
    ]

let rec buff ~depth ~dtype i1 o1 =
  if depth <= 0 then failwith "depth too low"
  else if Int.equal depth 1 then
    let chan1 = Chan.create dtype in
    let chan2 = Chan.create dtype in
    Chp.par [ split ~dtype i1 chan1.w chan2.w; merge ~dtype chan1.r chan2.r o1 ]
  else
    let chan1a = Chan.create dtype in
    let chan1b = Chan.create dtype in
    let chan2a = Chan.create dtype in
    let chan2b = Chan.create dtype in
    Chp.par
      [
        split ~dtype i1 chan1a.w chan2a.w;
        buff ~dtype ~depth:(depth - 1) chan1a.r chan1b.w;
        buff ~dtype ~depth:(depth - 1) chan2a.r chan2b.w;
        merge ~dtype chan1b.r chan2b.r o1;
      ]

let%expect_test "test_buff 1" =
  let dtype = CInt.dtype_32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:1 ~dtype i o) in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ i.u ]
      ~user_readable_ports:[ o.u ]
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
     "User send did not complete:  called in test/ir_test.ml on line 288, on chan created in test/ir_test.ml on line 253.") |}]

let%expect_test "test_buff 2" =
  let dtype = CInt.dtype_32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ i.u ]
      ~user_readable_ports:[ o.u ]
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
     "User send did not complete:  called in test/ir_test.ml on line 313, on chan created in test/ir_test.ml on line 297.") |}]

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
     "Mem access out of bounds: in test/ir_test.ml on line 349, idx is 4, size of mem is 4.") |}]

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
     "Simulatnious accesses of a memory/rom: statement 1 in test/ir_test.ml on line 379, statement 2 in test/ir_test.ml on line 374.") |}]

let%expect_test "test probes" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir =
    Chp.par
      [
        Chp.seq
          [
            Chp.log "A ";
            Chp.wait_probe_w chan.w;
            Chp.log "B ";
            Chp.log "C ";
            Chp.send chan.w CInt.E.(of_int 3);
            Chp.log "D ";
            Chp.log "E ";
            Chp.log "F ";
          ];
        Chp.seq
          [
            Chp.log "1 ";
            Chp.log "2 ";
            Chp.log "3 ";
            Chp.log "4 ";
            Chp.read chan.r var;
            Chp.log "5 ";
          ];
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan.w.u ]
      ~user_readable_ports:[] ~seed:777
  in
  Sim.wait' sim ();
  [%expect {|
    A 1 2 3 4 B C D E 5 F (Ok ()) |}]

module Op : sig
  type t = Add | Mul | And | Or [@@deriving sexp, equal, hash, compare]

  include CEnum.S with type t := t
end = struct
  module T = struct
    type t = Add | Mul | And | Or [@@deriving sexp, equal, hash, compare]

    let mapping =
      [
        (Add, CInt.of_int 0);
        (Mul, CInt.of_int 1);
        (And, CInt.of_int 2);
        (Or, CInt.of_int 3);
      ]
  end

  include T
  include CEnum.Make (T)
end

module Mini_alu = struct
  let val_dtype = CInt.dtype ~bits:8

  let alu =
    let op = Chan.create Op.dtype in
    let arg0 = Chan.create val_dtype in
    let arg1 = Chan.create val_dtype in
    let result = Chan.create val_dtype in
    let op_v = Var.create Op.dtype in
    let arg0_v = Var.create val_dtype in
    let arg1_v = Var.create val_dtype in
    let read_2_args =
      Chp.par [ Chp.read arg0.r arg0_v; Chp.read arg1.r arg1_v ]
    in
    let ir =
      Chp.loop
        [
          Chp.read op.r op_v;
          Op.Chp.match_
            Expr.(var op_v)
            ~f:(fun op_code ->
              let read_args = read_2_args in
              let send_result =
                match op_code with
                | Op.Add ->
                    let expr = CInt.E.(add (var arg0_v) (var arg1_v)) in
                    CInt.Chp.send result.w expr ~overflow:Mask
                | Op.Mul ->
                    let expr = CInt.E.(mul (var arg0_v) (var arg1_v)) in
                    CInt.Chp.send result.w expr ~overflow:Mask
                | Op.And ->
                    Chp.send result.w CInt.E.(bit_and (var arg0_v) (var arg1_v))
                | Op.Or ->
                    Chp.send result.w CInt.E.(bit_or (var arg0_v) (var arg1_v))
              in
              Chp.seq [ read_args; send_result ]);
        ]
    in
    (ir, op.w, arg0.w, arg1.w, result.r)
end

let%expect_test "mini cpu" =
  let ir, op, arg0, arg1, result = Mini_alu.alu in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
      ~user_readable_ports:[ result.u ]
  in
  Sim.wait' sim ();
  [%expect {|
    (Ok ()) |}];

  Sim.send sim op Add;
  Sim.send sim arg0 (CInt.of_int 3);
  Sim.send sim arg1 (CInt.of_int 7);
  Sim.read sim result (CInt.of_int 10);
  Sim.wait' sim ();
  [%expect {| (Ok ()) |}]

let%expect_test "error send too big value" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      let ir, op, arg0, arg1, result = Mini_alu.alu in
      let sim =
        Sim.simulate_chp ir ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
          ~user_readable_ports:[ result.u ]
      in
      Sim.send sim arg0 (CInt.of_int 1000));
  [%expect
    {|
    (Failure
     "Value doesnt fit in chan: got value 1000 but channel has layout (Bits_fixed 8).") |}]

let%expect_test "error read too big value" =
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      let ir, op, arg0, arg1, result = Mini_alu.alu in
      let sim =
        Sim.simulate_chp ir ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
          ~user_readable_ports:[ result.u ]
      in
      Sim.read sim result (CInt.of_int 257));
  [%expect
    {|
    (Failure
     "Value doesnt fit in chan: got value 257 but channel has layout (Bits_fixed 8).") |}]

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
     "Assigned value doesnt fit in var: got 190 but variable has layout (Bits_fixed 6) at in test/ir_test.ml on line 550.") |}]
