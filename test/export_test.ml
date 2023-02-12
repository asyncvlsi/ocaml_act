open! Core
open! Act

(* TODO switch stuff 4.14.0
   opam install bignum core dune expect_test_helpers_core ocamlfmt
*)

let%expect_test "test1" =
  let ir =
    let var0 = Var.create CInt.dtype_32 in
    let var1 = Var.create CInt.dtype_32 in
    let chan2 = Chan.create CInt.dtype_32 in
    N.seq
      [
        N.assign var0 CInt.E.(cint 12345);
        N.par
          [
            N.loop [ N.send chan2.w CInt.E.(var var0) ];
            N.seq [ N.read chan2.r var1; N.log1 var1 ~f:CInt.to_string ];
          ];
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc() {
      chan(int<32>) C0;
      int<32> v0;
      int<32> v1;


    chp {


    (v0 := 12345); (( *[C0!((v0))] ), (C0?v1))
    }
    } |}];
  let exporter =
    Exporter.create_dflow ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.dflow);
  [%expect
    {|
    (Seq
     ((Assign ((id 0) (bitwidth 32)) (Const 12345))
      (Par
       (((in_v ((id 0) (bitwidth 32))) (out_vs ((((id 2) (bitwidth 32))) ()))))
       ((DoWhile
         (((init_v (((id 2) (bitwidth 32)))) (body_in_v (((id 1) (bitwidth 32))))
           (body_out_v (((id 1) (bitwidth 32)))) (out_v ())))
         (Send ((id 0) (bitwidth 32)) (Var ((id 1) (bitwidth 32)))) (Const 1))
        (Read ((id 0) (bitwidth 32)) ((id 3) (bitwidth 32))))
       (((in_vs (() (((id 3) (bitwidth 32))))) (out_v ((id 4) (bitwidth 32))))))))
    ((Assign ((id 0) (bitwidth 32)) (Const 12345))
     (Assign ((id 1) (bitwidth 32)) (Var ((id 0) (bitwidth 32))))
     (Assign ((id 3) (bitwidth 32)) (Var ((id 2) (bitwidth 32))))
     (Assign ((id 4) (bitwidth 1)) (Const 1))
     (Merge ((id 5) (bitwidth 1)) (((id 1) (bitwidth 32)) ((id 6) (bitwidth 32)))
      ((id 7) (bitwidth 32)))
     (Split ((id 4) (bitwidth 1)) ((id 7) (bitwidth 32))
      (() (((id 6) (bitwidth 32)))))
     (Copy_init ((id 5) (bitwidth 1)) ((id 4) (bitwidth 1)) 0)
     (Assign ((id 8) (bitwidth 32)) (Var ((id 7) (bitwidth 32))))
     (Copy_init ((id 9) (bitwidth 1)) ((id 10) (bitwidth 1)) 0)
     (Assign ((id 10) (bitwidth 1))
      (BitXor (Const 1) (Var ((id 9) (bitwidth 1)))))
     (Assign ((id 12) (bitwidth 1)) (Var ((id 9) (bitwidth 1))))
     (SplitBoolGuard ((id 12) (bitwidth 1)) ((id 13) (bitwidth 1))
      (() (((id 15) (bitwidth 1)))))
     (MergeBoolGuard ((id 12) (bitwidth 1))
      (((id 4) (bitwidth 1)) ((id 15) (bitwidth 1))) ((id 14) (bitwidth 1)))
     (Assign ((id 16) (bitwidth 1))
      (BitOr (Var ((id 12) (bitwidth 1)))
       (BitXor (Var ((id 13) (bitwidth 1))) (Const 1))))
     (SplitBoolGuard ((id 16) (bitwidth 1)) ((id 12) (bitwidth 1))
      (() (((id 11) (bitwidth 1)))))
     (Copy_init ((id 13) (bitwidth 1)) ((id 14) (bitwidth 1)) 0)
     (Assign ((id 2) (bitwidth 32)) (Var ((id 8) (bitwidth 32))))
     (Copy_init ((id 17) (bitwidth 1)) ((id 18) (bitwidth 1)) 0)
     (Assign ((id 18) (bitwidth 1))
      (BitXor (Const 1) (Var ((id 17) (bitwidth 1)))))) |}]

let%expect_test "test2" =
  let ir =
    let var0 = Var.create CInt.dtype_32 ~init:(CInt.of_int 123456) in
    let var1 = Var.create CInt.dtype_32 ~init:(CInt.of_int 1) in
    N.seq
      [
        N.while_loop
          CInt.E.(ne (var var0) (cint 1))
          [
            CInt.N.assign var1 CInt.E.(var var1 |> add (cint 1)) ~overflow:Cant;
            N.if_else
              CInt.E.(mod_ (var var0) (cint 2) |> eq (cint 0))
              [ N.assign var0 CInt.E.(div (var var0) (cint 2)) ]
              [
                CInt.N.assign var0
                  CInt.E.(var var0 |> mul (cint 3) |> add (cint 1))
                  ~overflow:Cant;
              ];
          ];
        N.log1 var1 ~f:(fun v -> [%string "%{v#CInt}\n"]);
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc() {

      int<32> v0;
      int<32> v1;


    chp {
    v0 := 123456;
        v1 := 1;

     *[ bool(int((v0) != 1)) -> (v1 := (1 + (v1))); ([bool(int(0 = ((v0) % 2))) -> v0 := ((v0) / 2) [] else -> v0 := (1 + (3 * (v0)))]) ]
    }
    } |}]

let%expect_test "test3" =
  let dtype = CInt.dtype ~bits:9 in
  let var = Var.create dtype in
  let chan = Chan.create dtype in
  let ir =
    N.seq
      [
        N.log "start\n";
        N.read chan.r var;
        N.log "recv 1\n";
        N.read chan.r var;
        N.log "recv 2\n";
        N.read chan.r var;
        N.log "done\n";
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ chan.w.u ] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan?(int<9>) C0) {

      int<9> v0;


    chp {


    (C0?v0); (C0?v0); (C0?v0)
    }
    } |}]

let%expect_test "test3" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    N.seq
      [
        N.log "start\n";
        N.read chan1.r var1;
        N.log "recv 1\n";
        N.send chan2.w Expr.(var var1);
        N.log "send 1\n";
        N.assert_ CInt.E.(var var1 |> eq (cint 200));
        N.log "done\n";
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan!(int<32>) C1; chan?(int<32>) C0) {

      int<32> v0;


    chp {


    (C0?v0); (C1!((v0)))
    }
    } |}]

let%expect_test "test4" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir =
    N.seq
      [
        N.seq [];
        N.seq [];
        N.log "start\n";
        N.read chan1.r var1;
        N.seq [];
        N.log "recv 1\n";
        N.send chan2.w Expr.(var var1);
        N.log "send 1\n";
        N.assert_ CInt.E.(var var1 |> eq (cint 210));
        N.log "done\n";
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
      defproc my_proc(chan!(int<32>) C1; chan?(int<32>) C0) {

        int<32> v0;


      chp {


      (C0?v0); (C1!((v0)))
      }
      } |}]

let%expect_test "test5" =
  let var1 = Var.create CInt.dtype_32 in
  let chan1 = Chan.create CInt.dtype_32 in
  let chan2 = Chan.create CInt.dtype_32 in
  let ir = N.loop [ N.read chan1.r var1; N.send chan2.w Expr.(var var1) ] in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ chan1.w.u ]
      ~user_readable_ports:[ chan2.r.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan!(int<32>) C1; chan?(int<32>) C0) {

      int<32> v0;


    chp {


     *[(C0?v0); (C1!((v0)))]
    }
    } |}]

let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  N.loop
    [
      N.read i1 var1;
      N.if_else Expr.(var b1) [ N.send' o1 var1 ] [ N.send' o2 var1 ];
      CBool.N.toggle b1;
    ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  N.loop
    [
      N.if_else Expr.(var b1) [ N.read i1 var1 ] [ N.read i2 var1 ];
      N.send o1 Expr.(var var1);
      CBool.N.toggle b1;
    ]

let rec buff ~depth ~dtype i1 o1 =
  if depth <= 0 then failwith "depth too low"
  else if Int.equal depth 1 then
    let chan1 = Chan.create dtype in
    let chan2 = Chan.create dtype in
    N.par [ split ~dtype i1 chan1.w chan2.w; merge ~dtype chan1.r chan2.r o1 ]
  else
    let chan1a = Chan.create dtype in
    let chan1b = Chan.create dtype in
    let chan2a = Chan.create dtype in
    let chan2b = Chan.create dtype in
    N.par
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
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan!(int<32>) C1; chan?(int<32>) C0) {
      chan(int<32>) C2;
      chan(int<32>) C3;
      int<32> v0;
      int<1> v1;
      int<32> v2;
      int<1> v3;


    chp {
    v1 := 0;
        v3 := 0;

    ( *[(C0?v2); ([bool((v3)) -> C2!((v2)) [] else -> C3!((v2))]); (v3 := int((v3) = 0))] ), ( *[([bool((v1)) -> C2?v0 [] else -> C3?v0]); (C1!((v0))); (v1 := int((v1) = 0))] )
    }
    } |}]

let%expect_test "test_buff 2" =
  let dtype = CInt.dtype_32 in
  let i = Chan.W.create dtype in
  let o = Chan.R.create dtype in
  let ir = block11 i o ~f:(fun i o -> buff ~depth:2 ~dtype i o) in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan!(int<32>) C1; chan?(int<32>) C0) {
      chan(int<32>) C2;
      chan(int<32>) C3;
      chan(int<32>) C4;
      chan(int<32>) C5;
      chan(int<32>) C6;
      chan(int<32>) C7;
      chan(int<32>) C8;
      chan(int<32>) C9;
      int<32> v0;
      int<1> v1;
      int<32> v2;
      int<1> v3;
      int<32> v4;
      int<1> v5;
      int<32> v6;
      int<1> v7;
      int<32> v8;
      int<1> v9;
      int<32> v10;
      int<1> v11;


    chp {
    v1 := 0;
        v3 := 0;
        v5 := 0;
        v7 := 0;
        v9 := 0;
        v11 := 0;

    ( *[(C0?v10); ([bool((v11)) -> C2!((v10)) [] else -> C4!((v10))]); (v11 := int((v11) = 0))] ), ( *[(C2?v8); ([bool((v9)) -> C8!((v8)) [] else -> C9!((v8))]); (v9 := int((v9) = 0))] ), ( *[([bool((v7)) -> C8?v6 [] else -> C9?v6]); (C3!((v6))); (v7 := int((v7) = 0))] ), ( *[(C4?v4); ([bool((v5)) -> C6!((v4)) [] else -> C7!((v4))]); (v5 := int((v5) = 0))] ), ( *[([bool((v3)) -> C6?v2 [] else -> C7?v2]); (C5!((v2))); (v3 := int((v3) = 0))] ), ( *[([bool((v1)) -> C3?v0 [] else -> C5?v0]); (C1!((v0))); (v1 := int((v1) = 0))] )
    }
    } |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:CInt.E.(cint 3) ~dst:var1;
        N.log1 var1 ~f:CInt.to_string;
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    template<pint N; pint LN;  pint W> defproc ram(chan?(int<1>) Op; chan?(int<LN>) Idx; chan?(int<W>) WValue; chan!(int<W>) RValue) {
    int<W> mem[N]; int<1> op; int<LN> idx; int<W> tmp;
    chp { *[ Op?op, Idx?idx;
    [ op = 0 -> [ ([] i:N: idx = i -> RValue!(mem[i]) ) ]
    []  op = 1 -> WValue?tmp; [ ([] i:N: idx = i -> mem[i] := tmp ) ] ] ]
    } }

    defproc my_proc() {

      int<32> v0;
      ram<4, 2, 32> M0;

    chp {

    (M0.Op!1, M0.Idx!0, M0.WValue!1); (M0.Op!1, M0.Idx!1, M0.WValue!2); (M0.Op!1, M0.Idx!2, M0.WValue!3); (M0.Op!1, M0.Idx!3, M0.WValue!4);
    (M0.Op!0, M0.Idx!(3), M0.RValue?v0)
    }
    } |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let ir =
    N.seq
      [
        N.read_ug_mem mem ~idx:CInt.E.(cint 4) ~dst:var1;
        N.log1 var1 ~f:CInt.to_string;
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    template<pint N; pint LN;  pint W> defproc ram(chan?(int<1>) Op; chan?(int<LN>) Idx; chan?(int<W>) WValue; chan!(int<W>) RValue) {
    int<W> mem[N]; int<1> op; int<LN> idx; int<W> tmp;
    chp { *[ Op?op, Idx?idx;
    [ op = 0 -> [ ([] i:N: idx = i -> RValue!(mem[i]) ) ]
    []  op = 1 -> WValue?tmp; [ ([] i:N: idx = i -> mem[i] := tmp ) ] ] ]
    } }

    defproc my_proc() {

      int<32> v0;
      ram<4, 2, 32> M0;

    chp {

    (M0.Op!1, M0.Idx!0, M0.WValue!1); (M0.Op!1, M0.Idx!1, M0.WValue!2); (M0.Op!1, M0.Idx!2, M0.WValue!3); (M0.Op!1, M0.Idx!3, M0.WValue!4);
    (M0.Op!0, M0.Idx!(4), M0.RValue?v0)
    }
    } |}]

let%expect_test "mem" =
  let mem =
    let arr = [| 1; 2; 3; 4 |] |> Array.map ~f:CInt.of_int in
    Mem.create_ug_mem CInt.dtype_32 arr
  in
  let var1 = Var.create CInt.dtype_32 in
  let var2 = Var.create CInt.dtype_32 in
  let ir =
    N.par
      [
        N.seq
          [
            N.read_ug_mem mem ~idx:CInt.E.(cint 3) ~dst:var1;
            N.log1 var1 ~f:CInt.to_string;
          ];
        N.seq
          [
            N.read_ug_mem mem ~idx:CInt.E.(cint 3) ~dst:var2;
            N.log1 var2 ~f:CInt.to_string;
          ];
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    template<pint N; pint LN;  pint W> defproc ram(chan?(int<1>) Op; chan?(int<LN>) Idx; chan?(int<W>) WValue; chan!(int<W>) RValue) {
    int<W> mem[N]; int<1> op; int<LN> idx; int<W> tmp;
    chp { *[ Op?op, Idx?idx;
    [ op = 0 -> [ ([] i:N: idx = i -> RValue!(mem[i]) ) ]
    []  op = 1 -> WValue?tmp; [ ([] i:N: idx = i -> mem[i] := tmp ) ] ] ]
    } }

    defproc my_proc() {

      int<32> v0;
      int<32> v1;
      ram<4, 2, 32> M0;

    chp {

    (M0.Op!1, M0.Idx!0, M0.WValue!1); (M0.Op!1, M0.Idx!1, M0.WValue!2); (M0.Op!1, M0.Idx!2, M0.WValue!3); (M0.Op!1, M0.Idx!3, M0.WValue!4);
    ((M0.Op!0, M0.Idx!(3), M0.RValue?v0)), ((M0.Op!0, M0.Idx!(3), M0.RValue?v1))
    }
    } |}]

let%expect_test "test probes" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir =
    N.par
      [
        N.seq
          [
            N.log "A ";
            N.wait_probe_w chan.w;
            N.log "B ";
            N.log "C ";
            N.send chan.w CInt.E.(cint 3);
            N.log "D ";
            N.log "E ";
            N.log "F ";
          ];
        N.seq
          [
            N.log "1 ";
            N.log "2 ";
            N.log "3 ";
            N.log "4 ";
            N.read chan.r var;
            N.log "5 ";
          ];
      ]
  in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[] ~user_readable_ports:[]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc() {
      chan(int<32>) C0;
      int<32> v0;


    chp {


    (([ #C0]); (C0!(3))), (C0?v0)
    }
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
    let read_2_args = N.par [ N.read arg0.r arg0_v; N.read arg1.r arg1_v ] in
    let ir =
      N.loop
        [
          N.read op.r op_v;
          Op.N.match_
            Expr.(var op_v)
            ~f:(fun op_code ->
              let read_args = read_2_args in
              let send_result =
                match op_code with
                | Op.Add ->
                    let expr = CInt.E.(add (var arg0_v) (var arg1_v)) in
                    CInt.N.send result.w expr ~overflow:Mask
                | Op.Mul ->
                    let expr = CInt.E.(mul (var arg0_v) (var arg1_v)) in
                    CInt.N.send result.w expr ~overflow:Mask
                | Op.And ->
                    N.send result.w CInt.E.(bit_and (var arg0_v) (var arg1_v))
                | Op.Or ->
                    N.send result.w CInt.E.(bit_or (var arg0_v) (var arg1_v))
              in
              N.seq [ read_args; send_result ]);
        ]
    in
    (ir, op.w, arg0.w, arg1.w, result.r)
end

let%expect_test "mini cpu" =
  let ir, op, arg0, arg1, result = Mini_alu.alu in
  let exporter =
    Exporter.create ir ~user_sendable_ports:[ op.u; arg0.u; arg1.u ]
      ~user_readable_ports:[ result.u ]
  in
  ignore (exporter : Exporter.t);
  [%expect
    {|
    defproc my_proc(chan!(int<8>) C3; chan?(int<2>) C0; chan?(int<8>) C1; chan?(int<8>) C2) {

      int<2> v0;
      int<8> v1;
      int<8> v2;


    chp {


     *[(C0?v0); ([bool(int(0 = (v0))) -> ((C1?v1), (C2?v2)); (C3!((((v1) + (v2)) & 255))) [] bool(int(1 = (v0))) -> ((C1?v1), (C2?v2)); (C3!((((v1) * (v2)) & 255))) [] bool(int(2 = (v0))) -> ((C1?v1), (C2?v2)); (C3!(((v1) & (v2)))) [] bool(int(3 = (v0))) -> ((C1?v1), (C2?v2)); (C3!(((v1) | (v2))))])]
    }
    } |}]
