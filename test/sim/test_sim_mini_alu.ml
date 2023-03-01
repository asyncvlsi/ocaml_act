open! Core
open! Act

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
