open! Core
open! Act

let kmac_cjp ~bw ~kernel ~a ~leftinput ~command ~rightout ~out =
  assert (CInt.Chan.bw_le kernel bw);
  assert (CInt.Chan.bw_le a bw);
  assert (CInt.Chan.bw_le leftinput bw);
  assert (CInt.Chan.bw_le command bw);
  assert (CInt.Chan.bw_ge rightout bw);
  assert (CInt.Chan.bw_ge out bw);
  let val_dtype = CInt.dtype ~bits:bw in
  let inputx = Var.create val_dtype in
  let tininputx = Var.create val_dtype in
  let toutinputx = Var.create val_dtype in
  let outputx = Var.create val_dtype ~init:CInt.zero in
  let kernelV = Var.create val_dtype in
  let c = Var.create val_dtype in
  Chp.loop
    [
      Chp.read command c;
      Chp.select_imm
        [
          ( CInt.E.(eq (var c) (of_int 0)),
            CInt.Chp.assign ~overflow:Mask outputx
              CInt.E.(add (mul (var inputx) (var kernelV)) (var outputx)) );
          ( CInt.E.(eq (var c) (of_int 1)),
            Chp.seq
              [
                Chp.assign toutinputx CInt.E.(var tininputx);
                Chp.par
                  [
                    Chp.send_var rightout toutinputx;
                    Chp.read leftinput tininputx;
                  ];
              ] );
          ( CInt.E.(eq (var c) (of_int 5)),
            Chp.assign tininputx CInt.E.(var inputx) );
          (CInt.E.(eq (var c) (of_int 6)), Chp.send_var out outputx);
          (CInt.E.(eq (var c) (of_int 7)), Chp.read kernel kernelV);
          (CInt.E.(eq (var c) (of_int 8)), Chp.read a inputx);
          (CInt.E.(eq (var c) (of_int 9)), Chp.assign outputx CInt.E.zero);
        ]
        ~else_:None;
    ]

let%expect_test "kmac_cjp_test_1" =
  let bw = 8 in
  let kernel = Chan.create (CInt.dtype ~bits:bw) in
  let a = Chan.create (CInt.dtype ~bits:bw) in
  let leftinput = Chan.create (CInt.dtype ~bits:bw) in
  let command = Chan.create (CInt.dtype ~bits:bw) in
  let rightout = Chan.create (CInt.dtype ~bits:bw) in
  let out = Chan.create (CInt.dtype ~bits:bw) in
  let ir =
    kmac_cjp ~bw ~kernel:kernel.r ~a:a.r ~leftinput:leftinput.r
      ~command:command.r ~rightout:rightout.w ~out:out.w
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:[ kernel.w.u; a.w.u; leftinput.w.u; command.w.u ]
      ~user_readable_ports:[ rightout.r.u; out.r.u ]
  in
  Sim.wait' sim ();
  Sim.send sim command.w (CInt.of_int 6);
  Sim.read sim out.r (CInt.of_int 0);
  Sim.wait' sim ();
  [%expect {|
    (Ok ())
    (Ok ()) |}]
