open! Core
open! Act

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
  Exporter.export_chp ~as_dflow:true ir ~user_sendable_ports:[ ichan.w.u ]
    ~user_readable_ports:[ ochan.r.u ];

  [%expect
    {|
    defproc proc_0(chan!(int<8>) iport10; chan?(int<8>) oport9) {
      chan(int<8>) v5;
      chan(int<8>) v7;
      chan(int<8>) v9;
      chan(int<8>) v10;
    dataflow {
      dataflow_cluser {
        v7 <- ((v5) / 2);
        v9 <- ((v7) / 2);
        v5 <- ((v10) / 2);
      };
    iport10 -> v10;
    v9 -> oport9;
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
  Exporter.export_chp ~as_dflow:true ir ~user_sendable_ports:[ ichan.w.u ]
    ~user_readable_ports:[ ochan.r.u ];

  [%expect
    {|
    defproc proc_0(chan!(int<8>) iport10; chan?(int<8>) oport9) {
      chan(int<8>) v5;
      chan(int<8>) v7;
      chan(int<8>) v9;
      chan(int<8>) v10;
    dataflow {
      dataflow_cluser {
        v9 <- ((v5) / (v7));
        v5 <- ((v10) / 2);
        v7 <- ((v10) / 2);
      };
    iport10 -> v10;
    v9 -> oport9;
    }
    }


    defproc main(chan?(int<8>) user_i0;chan!(int<8>) user_o0) {

      chan(int<8>) c0;
      chan(int<8>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]
