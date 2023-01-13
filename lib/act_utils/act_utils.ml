open! Core
open! Random

module Any : sig
  type t [@@deriving sexp]
end = struct
  type t [@@deriving sexp]
end

(* printf "timed out = %s" (if timed_out then "YES" else "NO"); *)

(*
   module Spliter : sig
     module I = A1.CInt
     module O = A2.CInt

     val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A1.CInt
     module O = A2.CInt

     let chp ctx ~in_ ~(out : O.W.t) =
       let b = Var.new_bool ctx in
       let v = Var.new_of_dtype ctx (I.dtype in_) in
       Chp.loop
         [
           Chp.read in_ v;
           Chp.select_bool b ~f:(fun b ->
               let o = if not b then out.a0 else out.a1 in
               [ Chp.send_var o v ]);
           Chp.flip b;
         ]

     let inst ctx ~label ~in_ =
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp

     let%expect_test "chp block" =
       let dtype = DType.new_int ~max_bits:(Bits.of_int 3) in
       let ctx = Context.create_root () in
       let in_ = I.new_of_dtype ctx dtype in
       let out = O.W.new_of_dtype ctx dtype in
       let chp = chp ctx ~in_ ~out in
       Chp.hum_print_chp chp;
       [%expect
         {|
         (Loop
          ((Read
            (((ctx ((parent ()) (id 1))) (id 2) (dtype ((max_bits 3))))
             ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3))))))
           (BoolSelect ((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1))))
            ((Send
              (((ctx ((parent ()) (id 1))) (id 3) (dtype ((max_bits 3))))
               (Var ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3))))))))
            ((Send
              (((ctx ((parent ()) (id 1))) (id 4) (dtype ((max_bits 3))))
               (Var ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3)))))))))
           (Assign
            (((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1))))
             (BoolNot
              (Var ((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1)))))))))) |}]

     let sim ~max_bits =
       let dtype = DType.new_int ~max_bits in
       let ctx = Context.create_root () in
       let in_ = I.new_of_dtype ctx dtype in
       let in_w = I.get_writable_pair in_ in
       let out, node = inst ~label:"root" ctx ~in_ in
       (in_w, out, Sim.sim node)

     let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:
           [ Sim.D.new_w in_ (CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 3; 5; 7; 9 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 2; 4; 6; 8; 10 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 70, characters 10-61
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:[ Sim.D.new_w in_ (CInt.l_of_l [ 1; 0; 1; 0; 1 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 1; 1 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 0; 0 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 91, characters 10-51
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test1" =
       (* The circuit passes the value on as expected. We dont specify the order
          between the read and the write. The order between subsequent actions on
          the same channel is preserved, but there is no guarentee between
          different channels *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read sim out.a1 (CInt.of_int 321);
       Sim.read sim out.a0 (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 321);
       Sim.wait sim;
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 111, characters 4-41
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test2" =
       (* checks that we can send strictly before we can read *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 321);
       Sim.read_and_wait sim out.a1 (CInt.of_int 321);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 131, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test3" =
       (* checks that we cannot read before we send *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 150, characters 4-50
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test4" =
       (* checks that we cannot send twice before reading *)
       let in_, _out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 167, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
   end

   module Merger : sig
     module I = A2.CInt
     module O = A1.CInt

     val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A2.CInt
     module O = A1.CInt

     let chp ctx ~in_ ~out =
       let b = Var.new_bool ctx in
       let v = Var.new_of_dtype ctx (I.dtype in_) in
       Chp.loop
         [
           Chp.select_bool b ~f:(function
             | true -> [ Chp.read in_.a0 v ]
             | false -> [ Chp.read in_.a1 v ]);
           Chp.flip b;
           Chp.send out (Expr.of_var v);
         ]

     let inst ctx ~label ~in_ =
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp
   end

   module Buffer : sig
     module I = A1.CInt
     module O = A1.CInt

     val inst : Context.t -> depth:int -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A1.CInt
     module O = A1.CInt

     let rec inst ctx ~depth ~label ~in_ =
       let nodes ctx ~in_ ~out =
         if depth < 0 then failwith "depth <= 0"
         else if Int.equal depth 0 then [ Node.tie ctx in_ out ]
         else
           let split_out, split = Spliter.inst ctx ~label:"split" ~in_ in
           let buff0_out, buff0 =
             inst ctx ~depth:(depth - 1) ~label:"left" ~in_:split_out.a0
           in
           let buff1_out, buff1 =
             inst ctx ~depth:(depth - 1) ~label:"right" ~in_:split_out.a1
           in
           let out_, merge =
             Merger.inst ctx ~label:"merge"
               ~in_:{ Merger.I.a0 = buff0_out; a1 = buff1_out }
           in
           [ split; buff0; buff1; merge; Node.tie ctx out_ out ]
       in
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_cluster (module I) (module O) ctx ~label ~in_ ~out ~f:nodes
   end
*)

(*
   let sim ~depth ~mb =
     let in_ = I.new_ ~mb () in
     let out, node = inst ~depth ~in_:in_.r in
     ((in_.w, out), Sim.sim node)

   let%expect_test "test0" =
     let (in_, out), sim = sim ~depth:2 ~mb:(Bits.of_int 4) in
     let l =
       CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 7; 6; 6; 5; 4; 3; 2; 12; 1 ]
     in
     Sim.d_test sim ~inputs:[ Sim.D.new_w in_ l ] ~expected:[ Sim.D.new_r out l ];
     [%expect {||}]

   let%expect_test "test1" =
     let (in_, out), sim = sim ~depth:3 ~mb:(Bits.of_int 1) in
     let l = CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 0; 1; 2; 3; 4; 5; 6 ] in
     List.iter l ~f:(fun v -> Sim.send_and_wait sim in_ v);
     List.iter l ~f:(fun v -> Sim.read_and_wait sim out v);
     [%expect {||}] *)

(*
   module DBuffer = struct
     let rec inst ~depth ~in_ =
       if depth <= 0 then failwith "depth <= 0";
       let subinst ~in_ =
         match depth with 1 -> (in_, []) | _ -> inst ~depth:(depth - 1) ~in_
       in
       let split_out, split = Spliter.inst ~in_ in
       let buff1_out, buff1 = subinst ~in_:split_out.out_1 in
       let buff2_out, buff2 = subinst ~in_:split_out.out_2 in
       let out, merge = Merger.inst ~in_1:buff1_out ~in_2:buff2_out in
       (out, [ split; merge ] @ buff1 @ buff2)

     let inst ~depth ~in_ =
       let out, nodes = inst ~depth ~in_ in
       (out, Node_inst.of_subnodes nodes)

     let sim ~depth ~mb =
       let in_ = CInt.Chan.P.new_ ~mb () in
       let out, node = inst ~depth ~in_:in_.r in
       ((in_.w, out), Sim.sim node)

     let%expect_test "test0" =
       let (in_, out), sim = sim ~depth:2 ~mb:(Bits.of_int 4) in
       let l =
         CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 7; 6; 6; 5; 4; 3; 2; 12; 1 ]
       in
       Sim.d_test sim ~inputs:[ Sim.D.new_w in_ l ] ~expected:[ Sim.D.new_r out l ];
       [%expect {||}]

     let%expect_test "test1" =
       let (in_, out), sim = sim ~depth:3 ~mb:(Bits.of_int 1) in
       let l = CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 0; 1; 2; 3; 4; 5; 6 ] in
       List.iter l ~f:(fun v -> Sim.send_and_wait sim in_ v);
       List.iter l ~f:(fun v -> Sim.read_and_wait sim out v);
       [%expect {||}]
   end

   module ALU = struct
     module Instr = struct
       type t = Add | Sub | Mul | IDiv | UMinus | UNot

       let bits = Bits.of_int 4
       let all = [ Add; UMinus ]
       let arg_ct = function Add | Sub | Mul | IDiv -> 2 | UMinus | UNot -> 1

       let op_expr instr a0 a1 =
         match instr with
         | Add -> Expr.(add a0 a1)
         | Sub -> Expr.(sub a0 a1)
         | Mul -> Expr.(mul a0 a1)
         | IDiv -> Expr.(div a0 a1)
         | UMinus -> Expr.(uminus a1)
         | UNot -> Expr.(unot a0)

       module Expr = struct
         let of_var _ = failwith "unimplemented"
         let eq _ _ = failwith "unimplemented"
       end
     end

     let bits = Bits.of_int 16

     let inst ~instr ~arg_0 ~arg_1 =
       let open Chp in
       let res_0 = CInt.Chan.P.new_ ~mb:bits () in
       Block.create_3_1 ~in_:(instr, arg_0, arg_1) ~out:res_0
         ~f:(fun c_instr c_arg_0 c_arg_1 c_res_0 ->
           let instr = CInt.Var.new_ ~mb:Instr.bits () in
           let a0 = CInt.Var.new_ ~mb:bits () in
           let a1 = CInt.Var.new_ ~mb:bits () in
           let load_args instr =
             match Instr.arg_ct instr with
             | 1 -> Chp.read c_arg_0 a0
             | 2 -> Chp.par [ Chp.read c_arg_0 a0; Chp.read c_arg_1 a1 ]
             | _ -> failwith "unsupported"
           in
           let op_expr instr a0 a1 =
             Instr.op_expr instr (Expr.of_var a0) (Expr.of_var a1)
           in
           Chp.loop
             [
               Chp.read c_instr instr;
               Instr.Chp.match_ instr ~f:(fun instr ->
                   [ load_args instr; Chp.send c_res_0 (op_expr instr) ]);
             ])

     let inst () =
       let open Chp in
       let instr = CInt.Chan.P.new_ ~mb:Instr.bits () in
       let arg_0 = CInt.Chan.P.new_ ~mb:bits () in
       let arg_1 = CInt.Chan.P.new_ ~mb:bits () in
       let res_0, node = inst ~instr ~arg_0 ~arg_1 in
       ((instr, arg_0, arg_1, res_0), node)
   end
*)

(*
module Test_log : sig
  module I = A0
  module O = A0

  val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
end = struct
  module I = A0
  module O = A0

  let chp _ ~in_:_ ~out:_ = Chp.log (Expr.const "hello world")

  let inst ctx ~label ~in_ =
    let out = O.new_ ctx in
    Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp

  let%expect_test "chp block" =
    let ctx = Context.create_root () in
    let in_ = I.new_ ctx in
    let out = O.W.new_ ctx in
    let chp = chp ctx ~in_ ~out in
    Chp.hum_print_chp chp;
    [%expect {|
      (Log (Const "hello world")) |}]

  let sim ~max_bits:_ =
    let ctx = Context.create_root () in
    let in_ = I.new_ ctx in
    let in_w = I.get_writable_pair in_ in
    let out, node = inst ~label:"root" ctx ~in_ in
    (in_w, out, Sim.sim node)

  let%expect_test "test0" =
    let _, _, sim = sim ~max_bits:(Bits.of_int 4) in
    print_s [%sexp (sim : Sim.t)];
    [%expect
      {|
      ((program (Par ((Log (Const "hello world")))))
       (pc ((parent ()) (state (Index 0)) (children ())))
       (vars ((ids ()) (tbl ()))) (chans ((ids ()) (tbl ())))) |}];
    Sim.wait sim;
    [%expect
      {|
      ((parent ()) (state (Index 0)) (children ()))
      1
      (((parent ()) (state (Index 0)) (children ()))) |}]
  (* Sim.d_test sim ~inputs:[] ~expected:[]; *)

  (* let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:[ Sim.D.new_w in_ (CInt.l_of_l [ 1; 0; 1; 0; 1 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 1; 1 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 0; 0 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 93, characters 10-51
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test1" =
       (* The circuit passes the value on as expected. We dont specify the order
          between the read and the write. The order between subsequent actions on
          the same channel is preserved, but there is no guarentee between
          different channels *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read sim out.a1 (CInt.of_int 321);
       Sim.read sim out.a0 (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 321);
       Sim.wait sim;
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 114, characters 4-41
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test2" =
       (* checks that we can send strictly before we can read *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 321);
       Sim.read_and_wait sim out.a1 (CInt.of_int 321);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 135, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test3" =
       (* checks that we cannot read before we send *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 155, characters 4-50
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test4" =
       (* checks that we cannot send twice before reading *)
       let in_, _out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 173, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}] *)
end
*)
