open! Core
open! Ochp
open! Ochp.Act

let%expect_test "expression chains" =
  let ichan = Chan.create CInt.dtype_8 in
  let ochan = Chan.create CInt.dtype_8 in
  let ir =
    let tmp0 = Var.create CInt.dtype_8 in
    let tmp1 = Var.create CInt.dtype_8 in
    let tmp2 = Var.create CInt.dtype_8 in
    let tmp3 = Var.create CInt.dtype_8 in

    Chp.loop
      [
        Chp.read ichan.r tmp0;
        Chp.assign tmp1 Expr.(div (var tmp0) (of_int 2));
        Chp.assign tmp2 Expr.(div (var tmp1) (of_int 2));
        Chp.assign tmp3 Expr.(div (var tmp2) (of_int 2));
        Chp.send_var ochan.w tmp3;
      ]
  in
  Compiler.compile_chp ~to_:`Dataflow ir ~user_sendable_ports:[ ichan.w.u ]
    ~user_readable_ports:[ ochan.r.u ]
  |> Compiler.export_print;

  [%expect
    {|
    defproc proc_0(chan!(int<8>) iport3; chan?(int<8>) oport2) {
      chan(int<8>) v0;
      chan(int<8>) v1;
      chan(int<8>) v2;
      chan(int<8>) v3;
    dataflow {
      v0 <- ((((v1) / 2) / 2) / 2);
      v3 -> [1] v1;
      v0 -> [1] v2;
    iport3 -> v3;
    v2 -> oport2;
    }
    }


    defproc main(chan?(int<8>) user_i0;chan!(int<8>) user_o0) {

      chan(int<8>) c0;
      chan(int<8>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

let%expect_test "expression branches" =
  let ichan = Chan.create CInt.dtype_8 in
  let ochan = Chan.create CInt.dtype_8 in
  let ir =
    let tmp0 = Var.create CInt.dtype_8 in
    let tmp1 = Var.create CInt.dtype_8 in
    let tmp2 = Var.create CInt.dtype_8 in
    let tmp3 = Var.create CInt.dtype_8 in

    Chp.loop
      [
        Chp.read ichan.r tmp0;
        Chp.assign tmp1 Expr.(div (var tmp0) (of_int 2));
        Chp.assign tmp2 Expr.(div (var tmp0) (of_int 2));
        Chp.assign tmp3 Expr.(div (var tmp1) (var tmp2));
        Chp.send_var ochan.w tmp3;
      ]
  in
  Compiler.compile_chp ~to_:`Dataflow ir ~user_sendable_ports:[ ichan.w.u ]
    ~user_readable_ports:[ ochan.r.u ]
  |> Compiler.export_print;

  [%expect
    {|
    defproc proc_0(chan!(int<8>) iport3; chan?(int<8>) oport2) {
      chan(int<8>) v0;
      chan(int<8>) v1;
      chan(int<8>) v2;
      chan(int<8>) v3;
    dataflow {
      v0 <- (((v1) / 2) / ((v1) / 2));
      v3 -> [1] v1;
      v0 -> [1] v2;
    iport3 -> v3;
    v2 -> oport2;
    }
    }


    defproc main(chan?(int<8>) user_i0;chan!(int<8>) user_o0) {

      chan(int<8>) c0;
      chan(int<8>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]
