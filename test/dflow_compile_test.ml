open! Core
open! Act

let%expect_test "test3" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.seq [ Chp.read chan1.r var1; Chp.send chan2.w Expr.(var var1) ]
  in
  let sim =
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.send sim chan1.w (CInt.of_int 200);
  Sim.read sim chan2.r (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
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
        Chp.assert_ CInt.E.(var var1 |> eq (cint 210));
        Chp.log "done\n";
      ]
  in
  let sim =
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {|
    (Ok ()) |}];
  Sim.send sim chan1.w (CInt.of_int 200);
  Sim.wait' sim ();
  [%expect {|
    (Ok ()) |}];
  Sim.read sim chan2.r (CInt.of_int 200);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ()) |}]

let%expect_test "test5" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.loop [ Chp.read chan1.r var1; Chp.send chan2.w Expr.(var var1) ]
  in
  let sim =
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  Sim.wait' sim ();
  [%expect {|
    (Ok ()) |}];
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
       "User read has wrong value: got 4, but expected 5 based on `send' function call in test/dflow_compile_test.ml on line 79, on chan created in test/dflow_compile_test.ml on line 59.") |}]

let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.read i1 var1;
      Chp.if_else Expr.(var b1) [ Chp.send' o1 var1 ] [ Chp.send' o2 var1 ];
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
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[ i.u ]
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
    (Ok ()) |}]

let%expect_test "test_buff 2" =
  let dtype = CInt.dtype_32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  let sim =
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[ i.u ]
      ~user_readable_ports:[ o.u ]
  in

  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.send sim i (CInt.of_int v));
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  List.iter [ 1; 2; 3; 4; 5; 6 ] ~f:(fun v -> Sim.read sim o (CInt.of_int v));
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}]

module Op : sig
  type t = Add | Mul | And | Or [@@deriving sexp, equal, hash, compare]

  include Enum.S with type t := t
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
  include Enum.Make (T)
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
    Exporter.stf_sim ~optimize:true ir
      ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
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

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let ir =
    Chp.seq
      [
        Chp.read_ug_mem mem ~idx:CInt.E.(cint 3) ~dst:var1;
        Chp.log1 var1 ~f:CInt.to_string;
      ]
  in
  let sim =
    Exporter.stf_sim ~optimize:true ir ~user_sendable_ports:[]
      ~user_readable_ports:[]
  in
  Sim.wait' sim ();
  [%expect {|
    (Ok ()) |}]
