open! Core
open! Act

(* $MDX part-begin=colatz_example *)
let i = Chan.create CInt.dtype_32
let o = Chan.create CInt.dtype_32

let ir =
  let var0 = Var.create CInt.dtype_32 ~init:(CInt.of_int 123456) in
  let counter = Var.create CInt.dtype_32 ~init:(CInt.of_int 1) in
  Chp.loop
    [
      Chp.assign counter Expr.zero;
      Chp.read i.r var0;
      Chp.while_loop
        CInt.E.(ne (var var0) (of_int 1))
        [
          CInt.Chp.assign counter
            CInt.E.(var counter |> add (of_int 1))
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
      Chp.send_var o.w counter;
    ]

let%expect_test "colatz - chp" =
  let sim =
    Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
      ~user_readable_ports:[ o.r.u ] ~to_:`Chp
    |> Compiler.sim
  in
  Sim.send sim i.w (CInt.of_int 4);
  Sim.read sim o.r (CInt.of_int 2);
  Sim.wait' sim ();

  Sim.send sim i.w (CInt.of_int 12345);
  Sim.read sim o.r (CInt.of_int 50);
  Sim.wait' sim ();

  Sim.send sim i.w (CInt.of_int 13579753);
  Sim.read sim o.r (CInt.of_int 166);
  Sim.wait' ~max_steps:1000000 sim ();

  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ()) |}]

let%expect_test "colatz - dataflow" =
  let sim =
    Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
      ~user_readable_ports:[ o.r.u ] ~to_:`Dataflow
    |> Compiler.sim
  in
  Sim.send sim i.w (CInt.of_int 4);
  Sim.read sim o.r (CInt.of_int 2);
  Sim.wait' ~max_steps:1000000 sim ();

  Sim.send sim i.w (CInt.of_int 12345);
  Sim.read sim o.r (CInt.of_int 50);
  Sim.wait' ~max_steps:1000000 sim ();

  Sim.send sim i.w (CInt.of_int 13579753);
  Sim.read sim o.r (CInt.of_int 166);
  Sim.wait' ~max_steps:1000000 sim ();

  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ()) |}]

let%expect_test "colatz - chp - export" =
  Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
    ~user_readable_ports:[ o.r.u ] ~to_:`Chp
  |> Compiler.export |> printf "%s";
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
      int<32> v1;
    chp {
    (v1 := 123456); (v0 := 1); ( *[ ( [true] ); (v0 := 0); (C0?v1); ( [true] ); ([bool(int(int((v1) != 1) = 0)) ->  [true]  [] bool(int((v1) != 1)) ->  *[ ( [true] ); (v0 := (1 + (v0))); ( [true] ); ([bool(int(0 = ((v1) % 2))) -> ( [true] ); (v1 := ((v1) / 2)) [] bool(int(int(0 = ((v1) % 2)) = 0)) -> ( [true] ); (v1 := (1 + (3 * (v1))))]); ( [true] ) <- bool(int((v1) != 1)) ] ]); ( [true] ); (C1!((v0))) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

let%expect_test "colatz - dataflow - export" =
  Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
    ~user_readable_ports:[ o.r.u ] ~to_:`Chp
  |> Compiler.export |> printf "%s";
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
      int<32> v1;
    chp {
    (v1 := 123456); (v0 := 1); ( *[ ( [true] ); (v0 := 0); (C0?v1); ( [true] ); ([bool(int(int((v1) != 1) = 0)) ->  [true]  [] bool(int((v1) != 1)) ->  *[ ( [true] ); (v0 := (1 + (v0))); ( [true] ); ([bool(int(0 = ((v1) % 2))) -> ( [true] ); (v1 := ((v1) / 2)) [] bool(int(int(0 = ((v1) % 2)) = 0)) -> ( [true] ); (v1 := (1 + (3 * (v1))))]); ( [true] ) <- bool(int((v1) != 1)) ] ]); ( [true] ); (C1!((v0))) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]
(* $MDX part-end *)
