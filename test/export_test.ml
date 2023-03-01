open! Core
open! Act

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0() {
      chan(int<32>) C0;
      int<32> v0;
      int<32> v1;
    chp {
    (v0 := 0); (v1 := 0); ( [true] ); (v0 := 12345); (( *[ ( [true] ); (C0!((v0))) <- bool(1) ] ), (C0?v1))
    }
    }


    defproc main() {


    proc_0 proc_0_ ();

    } |}];
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0() {
      chan(int<32>) C0;
      int<32> v0;
      int<32> v1;
    chp {
    (v0 := 0); (v1 := 0); ( [true] ); (v0 := 12345); (( *[ ( [true] ); (C0!((v0))) <- bool(1) ] ), (C0?v1))
    }
    }


    defproc main() {


    proc_0 proc_0_ ();

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0() {

      int<32> v0;
      int<32> v1;
    chp {
    (v0 := 123456); (v1 := 1); ( [true] ); ([bool(int(int((v0) != 1) = 0)) ->  [true]  [] bool(int((v0) != 1)) ->  *[ ( [true] ); (v1 := (1 + (v1))); ( [true] ); ([bool(int(0 = ((v0) % 2))) -> ( [true] ); (v0 := ((v0) / 2)) [] bool(int(int(0 = ((v0) % 2)) = 0)) -> ( [true] ); (v0 := (1 + (3 * (v0))))]); ( [true] ) <- bool(int((v0) != 1)) ] ])
    }
    }


    defproc main() {


    proc_0 proc_0_ ();

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[ chan.w.u ]
    ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0(chan!(int<9>) C0) {

      int<9> v0;
    chp {
    (v0 := 0); (C0?v0); (C0?v0); (C0?v0)
    }
    }


    defproc main(chan?(int<9>) user_i0) {

      chan(int<9>) c0;
    proc_0 proc_0_ (c0);
    c0 = user_i0;
    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[ chan1.w.u ]
    ~user_readable_ports:[ chan2.r.u ];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
    chp {
    (v0 := 0); (C0?v0); ( [true] ); (C1!((v0))); ( [true] ); ( [true] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[ chan1.w.u ]
    ~user_readable_ports:[ chan2.r.u ];
  [%expect
    {|
      defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

        int<32> v0;
      chp {
      (v0 := 0); (C0?v0); ( [true] ); (C1!((v0))); ( [true] ); ( [true] )
      }
      }


      defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

        chan(int<32>) c0;
        chan(int<32>) c1;
      proc_0 proc_0_ (c0,c1);
      c0 = user_i0;
      c1 = user_o0;
      } |}]

let%expect_test "test5" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    Chp.loop [ Chp.read chan1.r var1; Chp.send chan2.w Expr.(var var1) ]
  in
  Exporter.export_chp ir ~user_sendable_ports:[ chan1.w.u ]
    ~user_readable_ports:[ chan2.r.u ];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
    chp {
    (v0 := 0); ( *[ (C0?v0); ( [true] ); (C1!((v0))) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[ i.u ]
    ~user_readable_ports:[ o.u ];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C3) {
      chan(int<32>) C1;
      chan(int<32>) C2;
      int<32> v0;
      int<1> v1;
      int<1> v2;
      int<32> v3;
    chp {
    (v3 := 0); (v2 := 0); (v0 := 0); (v1 := 0); (( *[ (C0?v0); ([bool((v1)) -> ( [true] ); (C1!((v0))) [] bool(int((v1) = 0)) -> ( [true] ); (C2!((v0)))]); ( [true] ); (v1 := int((v1) = 0)) <- bool(1) ] ), ( *[ ([bool((v2)) -> C1?v3 [] bool(int((v2) = 0)) -> C2?v3]); ( [true] ); (C3!((v3))); ( [true] ); (v2 := int((v2) = 0)) <- bool(1) ] ))
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

let%expect_test "test_buff 2" =
  let dtype = CInt.dtype_32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  Exporter.export_chp ir ~user_sendable_ports:[ i.u ]
    ~user_readable_ports:[ o.u ];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C9) {
      chan(int<32>) C1;
      chan(int<32>) C2;
      chan(int<32>) C3;
      chan(int<32>) C4;
      chan(int<32>) C5;
      chan(int<32>) C6;
      chan(int<32>) C7;
      chan(int<32>) C8;
      int<32> v0;
      int<1> v1;
      int<32> v2;
      int<1> v3;
      int<1> v4;
      int<32> v5;
      int<32> v6;
      int<1> v7;
      int<1> v8;
      int<32> v9;
      int<1> v10;
      int<32> v11;
    chp {
    (v11 := 0); (v10 := 0); (v9 := 0); (v8 := 0); (v6 := 0); (v7 := 0); (v5 := 0); (v4 := 0); (v2 := 0); (v3 := 0); (v0 := 0); (v1 := 0); (( *[ (C0?v0); ([bool((v1)) -> ( [true] ); (C1!((v0))) [] bool(int((v1) = 0)) -> ( [true] ); (C2!((v0)))]); ( [true] ); (v1 := int((v1) = 0)) <- bool(1) ] ), ( *[ (C1?v2); ([bool((v3)) -> ( [true] ); (C3!((v2))) [] bool(int((v3) = 0)) -> ( [true] ); (C4!((v2)))]); ( [true] ); (v3 := int((v3) = 0)) <- bool(1) ] ), ( *[ ([bool((v4)) -> C3?v5 [] bool(int((v4) = 0)) -> C4?v5]); ( [true] ); (C5!((v5))); ( [true] ); (v4 := int((v4) = 0)) <- bool(1) ] ), ( *[ (C2?v6); ([bool((v7)) -> ( [true] ); (C6!((v6))) [] bool(int((v7) = 0)) -> ( [true] ); (C7!((v6)))]); ( [true] ); (v7 := int((v7) = 0)) <- bool(1) ] ), ( *[ ([bool((v8)) -> C6?v9 [] bool(int((v8) = 0)) -> C7?v9]); ( [true] ); (C8!((v9))); ( [true] ); (v8 := int((v8) = 0)) <- bool(1) ] ), ( *[ ([bool((v10)) -> C5?v11 [] bool(int((v10) = 0)) -> C8?v11]); ( [true] ); (C9!((v11))); ( [true] ); (v10 := int((v10) = 0)) <- bool(1) ] ))
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<3>) C1) {

      int<32> v0;
    chp {
    (v0 := 0); ( [true] ); ((C1!((3 << 1))), (C0?v0))
    }
    }

    defproc proc_1(chan?(int<3>>) cmd_chan; chan?(int<32>>) read_chan) {

    int<32> v[4];
    int<3> cmd;
    int<3> tmp;
    chp {
    v[0] := 1;v[1] := 2;v[2] := 3;v[3] := 4;
    *[
    cmd_chan?cmd;
    [ cmd & 1 = 0 ->
    [
    ([]: j:4:  (cmd >> 1) = j   ->  tmp := v[j]  )
    ];
    read_chan!tmp
    [] cmd & 1 = 1 ->
    skip

    ]
    ]
    }
    }


    defproc main() {

      chan(int<32>) c0;
      chan(int<3>) c1;
    proc_0 proc_0_ (c0,c1);
    proc_1 proc_1_ (c1,c0);

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<3>) C1) {

      int<32> v0;
    chp {
    (v0 := 0); ( [true] ); ((C1!((4 << 1))), (C0?v0))
    }
    }

    defproc proc_1(chan?(int<3>>) cmd_chan; chan?(int<32>>) read_chan) {

    int<32> v[4];
    int<3> cmd;
    int<3> tmp;
    chp {
    v[0] := 1;v[1] := 2;v[2] := 3;v[3] := 4;
    *[
    cmd_chan?cmd;
    [ cmd & 1 = 0 ->
    [
    ([]: j:4:  (cmd >> 1) = j   ->  tmp := v[j]  )
    ];
    read_chan!tmp
    [] cmd & 1 = 1 ->
    skip

    ]
    ]
    }
    }


    defproc main() {

      chan(int<32>) c0;
      chan(int<3>) c1;
    proc_0 proc_0_ (c0,c1);
    proc_1 proc_1_ (c1,c0);

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<3>) C1) {

      int<32> v0;
      int<32> v1;
    chp {
    (v0 := 0); (v1 := 0); ((( [true] ); ((C1!((3 << 1))), (C0?v0))), (( [true] ); ((C1!((3 << 1))), (C0?v1))))
    }
    }

    defproc proc_1(chan?(int<3>>) cmd_chan; chan?(int<32>>) read_chan) {

    int<32> v[4];
    int<3> cmd;
    int<3> tmp;
    chp {
    v[0] := 1;v[1] := 2;v[2] := 3;v[3] := 4;
    *[
    cmd_chan?cmd;
    [ cmd & 1 = 0 ->
    [
    ([]: j:4:  (cmd >> 1) = j   ->  tmp := v[j]  )
    ];
    read_chan!tmp
    [] cmd & 1 = 1 ->
    skip

    ]
    ]
    }
    }


    defproc main() {

      chan(int<32>) c0;
      chan(int<3>) c1;
    proc_0 proc_0_ (c0,c1);
    proc_1 proc_1_ (c1,c0);

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[] ~user_readable_ports:[];
  [%expect
    {|
    defproc proc_0() {
      chan(int<32>) C0;
      int<32> v0;
    chp {
    (v0 := 0); ((([| #C0 ->  [true]  |]); ( [true] ); (C0!(3))), (C0?v0))
    }
    }


    defproc main() {


    proc_0 proc_0_ ();

    } |}]

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
  Exporter.export_chp ir ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
    ~user_readable_ports:[ result.u ];
  [%expect
    {|
    defproc proc_0(chan!(int<2>) C0; chan!(int<8>) C1; chan!(int<8>) C2; chan?(int<8>) C3) {

      int<2> v0;
      int<8> v1;
      int<8> v2;
    chp {
    (v0 := 0); (v1 := 0); (v2 := 0); ( *[ (C0?v0); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ( [true] ); ([bool(int(0 = (v0))) -> ((C1?v1), (C2?v2)); ( [true] ); (C3!((((v1) + (v2)) & 255))) [] bool(int(1 = (v0))) -> ((C1?v1), (C2?v2)); ( [true] ); (C3!((((v1) * (v2)) & 255))) [] bool(int(2 = (v0))) -> ((C1?v1), (C2?v2)); ( [true] ); (C3!(((v1) & (v2)))) [] bool(int(3 = (v0))) -> ((C1?v1), (C2?v2)); ( [true] ); (C3!(((v1) | (v2))))]) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<2>) user_i0;chan?(int<8>) user_i1;chan?(int<8>) user_i2;chan!(int<8>) user_o0) {

      chan(int<2>) c0;
      chan(int<8>) c1;
      chan(int<8>) c2;
      chan(int<8>) c3;
    proc_0 proc_0_ (c0,c1,c2,c3);
    c0 = user_i0;
    c1 = user_i1;
    c2 = user_i2;
    c3 = user_o0;
    } |}]
