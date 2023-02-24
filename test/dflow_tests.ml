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
    (DoWhile ()
     (Seq
      ((Read ((id 0) (bitwidth 8)) ((id 4) (bitwidth 8)))
       (Assign ((id 5) (bitwidth 8)) (Div (Var ((id 4) (bitwidth 8))) (Const 2)))
       (Assign ((id 6) (bitwidth 8)) (Div (Var ((id 5) (bitwidth 8))) (Const 2)))
       (Assign ((id 7) (bitwidth 8)) (Div (Var ((id 6) (bitwidth 8))) (Const 2)))
       (Send ((id 1) (bitwidth 8)) (Var ((id 7) (bitwidth 8))))))
     (Const 1))
    defproc proc_0(chan!(int<8>) iport6; chan?(int<8>) oport5) {
      chan(int<8>) v3;
      chan(int<8>) v4;
      chan(int<8>) v5;
      chan(int<8>) v6;
    dataflow {
      dataflow_cluser {
        v3 <- ((v6) / 2);
        v4 <- ((v3) / 2);
        v5 <- ((v4) / 2);
      };
    iport6 -> v6;
    v5 -> oport5;
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
    (DoWhile ()
     (Seq
      ((Read ((id 0) (bitwidth 8)) ((id 4) (bitwidth 8)))
       (Assign ((id 5) (bitwidth 8)) (Div (Var ((id 4) (bitwidth 8))) (Const 2)))
       (Assign ((id 6) (bitwidth 8)) (Div (Var ((id 4) (bitwidth 8))) (Const 2)))
       (Assign ((id 7) (bitwidth 8))
        (Div (Var ((id 5) (bitwidth 8))) (Var ((id 6) (bitwidth 8)))))
       (Send ((id 1) (bitwidth 8)) (Var ((id 7) (bitwidth 8))))))
     (Const 1))
    defproc proc_0(chan!(int<8>) iport6; chan?(int<8>) oport5) {
      chan(int<8>) v3;
      chan(int<8>) v4;
      chan(int<8>) v5;
      chan(int<8>) v6;
    dataflow {
      dataflow_cluser {
        v3 <- ((v6) / 2);
        v4 <- ((v6) / 2);
        v5 <- ((v3) / (v4));
      };
    iport6 -> v6;
    v5 -> oport5;
    }
    }


    defproc main(chan?(int<8>) user_i0;chan!(int<8>) user_o0) {

      chan(int<8>) c0;
      chan(int<8>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]
