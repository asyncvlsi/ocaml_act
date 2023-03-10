open! Core
open! Act

module Instr = struct
  (* TODO autogenerate this with a ppx *)
  module T = struct
    type t =
      | End
      | Nop
      | Push_imm
        (* push the next instruction, and then increase the program counter by
           2 *)
      | Push_imm2
      | Dup (* pop a; push a; push a *)
      | Exch (* pop a; pop b; push a; push b *)
      | Exch2
        (* pop a; pop b; pop c; push a; push c; push b; [TOP;a;b;c] ->
           [TOP;b;c;a] *)
      | Jump (* pop addr_high; pop addr_loc; goto addr *)
      | JumpIfNot
        (* pop addr_high; pop addr_loc; pop flag; if (flag == 0) then goto
           addr *)
      | Eq (* pop a; pop b; push Int8.(a == b) *)
      | Add (* pop a; pop b; push Int8.Wrapping.(a + b) *)
      | Sub (* pop a; pop b; push Int8.Wrapping.(b - a) *)
      | Bool_or (* pop a; pop b; push (a != 0 || b != 0) *)
      | Bool_not (* pop a; pop b; push (a == 0) *)
      | Output (* pop a; send_output a *)
      | Input (* read_input a; push a *)
    [@@deriving sexp, equal, hash, compare]

    let mapping =
      [
        End;
        Nop;
        Push_imm;
        Push_imm2;
        Dup;
        Exch;
        Exch2;
        Jump;
        JumpIfNot;
        Eq;
        Add;
        Sub;
        Output;
        Input;
        Bool_not;
        Bool_or;
      ]
      |> List.mapi ~f:(fun i e -> (e, CInt.of_int i))
  end

  include T
  include CEnum.Make (T)
end

let cpu instrs ~ochan ~ichan =
  let tmp0 = Var.create CInt.dtype_8 in
  let arg = Var.create CInt.dtype_4 in
  let instr = Var.create Instr.dtype in

  (* let tmp1 = Var.create CInt.dtype_8 in *)
  (* let tmp2 = Var.create CInt.dtype_8 in *)

  (* let addr_high = Var.create CInt.dtype_8 in *)
  (* let addr_low = Var.create CInt.dtype_8 in *)
  (* let flag = Var.create CInt.dtype_8 in *)

  (* program code *)
  let instrs = Mem.create_ug_rom CInt.dtype_8 instrs in
  let pc = Var.create (CInt.dtype ~bits:12) ~init:(CInt.of_int 0) in

  (* stack *)
  let stack = Array.init 4096 ~f:(fun _ -> CInt.zero) in
  let stack = Mem.create_ug_mem CInt.dtype_8 stack in
  let sp = Var.create (CInt.dtype ~bits:12) ~init:(CInt.of_int 0) in

  let pop_ct = Var.create CInt.dtype_8 in
  let pop0 = Var.create CInt.dtype_8 in
  let pop1 = Var.create CInt.dtype_8 in
  let pop2 = Var.create CInt.dtype_8 in

  let push_ct = Var.create CInt.dtype_8 in
  let push0 = Var.create CInt.dtype_8 in
  let push1 = Var.create CInt.dtype_8 in
  let push2 = Var.create CInt.dtype_8 in

  (* ir *)
  let done_ = Var.create CBool.dtype ~init:false in
  let push value =
    Chp.seq
      [
        (* Chp.log1' value ~f:(fun value -> sprintf "push %d\n" (CInt.to_int_exn
           value)); *)
        Chp.write_ug_mem stack ~idx:CInt.E.(var sp) ~value;
        CInt.Chp.incr sp ~overflow:Cant;
      ]
  in
  let pop ~dst =
    Chp.seq
      [
        CInt.Chp.decr sp ~underflow:Cant;
        Chp.read_ug_mem stack ~idx:CInt.E.(var sp) ~dst;
        (* Chp.log1 dst ~f:(fun dst -> sprintf "pop %d\n" (CInt.to_int_exn
           dst)); *)
      ]
  in
  let set_pc_to_addr ~addr_high ~addr_low =
    let expr =
      CInt.E.(
        left_shift (var addr_high) ~amt:(of_int 8)
        |> bit_or (var addr_low)
        |> clip ~bits:12)
    in
    Chp.assign pc expr
  in

  Chp.while_loop
    CBool.E.(not_ (var done_))
    [
      (* These helps the compiler optimize away the loping variable. TODO Maybe
         make this something like "forget" *)
      Chp.assign pop0 Expr.zero;
      Chp.assign pop1 Expr.zero;
      Chp.assign pop2 Expr.zero;
      Chp.assign push0 Expr.zero;
      Chp.assign push1 Expr.zero;
      Chp.assign push2 Expr.zero;
      (* TODO I would also like to be able to put them at the bottom of the loop
         instead of the top *)
      Chp.read_ug_rom instrs ~idx:Expr.(var pc) ~dst:tmp0;
      Chp.assign instr
        (Expr.var tmp0 |> Expr.bit_and (Expr.of_int 0xf) |> Instr.E.of_int);
      Chp.assign arg (Expr.var tmp0 |> Expr.right_shift' ~amt:4);
      (* Chp.log "\n"; *)
      (* Chp.log1 pc ~f:(fun pc -> sprintf "pc = %d\n" (CInt.to_int_exn pc)); *)
      (* Chp.log1 tmp0 ~f:(fun instr -> sprintf "instr = %d\n" (CInt.to_int_exn
         instr)); *)
      (* Chp.log1 sp ~f:(fun sp -> sprintf "sp = %d\n" (CInt.to_int_exn sp)); *)
      Instr.Chp.match_ (Expr.var instr) ~f:(fun op ->
          let pop_ct_i =
            match op with
            | End -> 0
            | Nop -> 0
            | Push_imm -> 0
            | Push_imm2 -> 1
            | Dup -> 1
            | Exch -> 2
            | Exch2 -> 3
            | Jump -> 2
            | JumpIfNot -> 3
            | Eq -> 2
            | Add -> 2
            | Sub -> 2
            | Bool_or -> 2
            | Bool_not -> 1
            | Input -> 0
            | Output -> 1
          in
          Chp.seq [ Chp.assign pop_ct Expr.(of_int pop_ct_i) ]);
      Chp.if_else Expr.(ge (var pop_ct) (of_int 1)) [ pop ~dst:pop0 ] [];
      Chp.if_else Expr.(ge (var pop_ct) (of_int 2)) [ pop ~dst:pop1 ] [];
      Chp.if_else Expr.(ge (var pop_ct) (of_int 3)) [ pop ~dst:pop2 ] [];
      Instr.Chp.match_ (Expr.var instr) ~f:(fun op ->
          let push_ct_i =
            match op with
            | End -> 0
            | Nop -> 0
            | Push_imm -> 1
            | Push_imm2 -> 1
            | Dup -> 2
            | Exch -> 2
            | Exch2 -> 3
            | Jump -> 0
            | JumpIfNot -> 0
            | Eq -> 1
            | Add -> 1
            | Sub -> 1
            | Bool_or -> 1
            | Bool_not -> 1
            | Input -> 1
            | Output -> 0
          in
          Chp.seq [ Chp.assign push_ct Expr.(of_int push_ct_i) ]);
      Instr.Chp.match_ (Expr.var instr) ~f:(function
        | End -> Chp.assign done_ CBool.E.true_
        | Nop -> CInt.Chp.incr pc ~overflow:Cant
        | Push_imm ->
            (* push the next instruction, and then increase the program counter
               by 2 *)
            Chp.seq
              [
                Chp.assign push0 Expr.(var arg); CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Push_imm2 ->
            (* push the next instruction, and then increase the program counter
               by 2 *)
            Chp.seq
              [
                Chp.assign push0
                  Expr.(
                    var pop0 |> clip ~bits:4 |> left_shift' ~amt:4
                    |> bit_or (var arg));
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Dup ->
            Chp.seq
              [
                Chp.assign push0 Expr.(var pop0);
                Chp.assign push1 Expr.(var pop0);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Exch ->
            Chp.seq
              [
                Chp.assign push0 Expr.(var pop0);
                Chp.assign push1 Expr.(var pop1);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Exch2 ->
            Chp.seq
              [
                Chp.assign push0 Expr.(var pop0);
                Chp.assign push1 Expr.(var pop2);
                Chp.assign push2 Expr.(var pop1);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Jump -> Chp.seq [ set_pc_to_addr ~addr_high:pop0 ~addr_low:pop1 ]
        | JumpIfNot ->
            (* pop flag; pop addr_high; pop addr_loc; if (flag == 0) then goto
               addr *)
            Chp.seq
              [
                CBool.Chp.match_
                  CInt.E.(eq (var pop2) (of_int 0))
                  ~f:(function
                    | true -> set_pc_to_addr ~addr_high:pop0 ~addr_low:pop1
                    | false -> CInt.Chp.incr pc ~overflow:Cant);
              ]
        | Eq ->
            Chp.seq
              [
                Chp.assign push0
                  (CInt.E.(eq (var pop0) (var pop1)) |> CBool.E.to_int);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Add ->
            Chp.seq
              [
                Chp.assign push0 CInt.E.(add_wrap (var pop0) (var pop1) ~bits:8);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Sub ->
            Chp.seq
              [
                Chp.assign push0 CInt.E.(sub_wrap (var pop1) (var pop0) ~bits:8);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Bool_or ->
            Chp.seq
              [
                Chp.assign push0
                  CInt.E.(
                    bit_or
                      (var pop0 |> ne zero |> CBool.E.to_int)
                      (var pop1 |> ne zero |> CBool.E.to_int));
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Bool_not ->
            Chp.seq
              [
                Chp.assign push0 CInt.E.(var pop0 |> eq zero |> CBool.E.to_int);
                CInt.Chp.incr pc ~overflow:Cant;
              ]
        | Input ->
            Chp.seq [ Chp.read ichan push0; CInt.Chp.incr pc ~overflow:Cant ]
        | Output ->
            Chp.seq
              [
                Chp.send ochan (Expr.var pop0); CInt.Chp.incr pc ~overflow:Cant;
              ]);
      Chp.if_else
        Expr.(ge (var push_ct) (of_int 1))
        [ push Expr.(var push0) ]
        [];
      Chp.if_else
        Expr.(ge (var push_ct) (of_int 2))
        [ push Expr.(var push1) ]
        [];
      Chp.if_else
        Expr.(ge (var push_ct) (of_int 3))
        [ push Expr.(var push2) ]
        [];
    ]

let test instrs ~create =
  let my_instrs =
    Array.init 4096 ~f:(fun i ->
        if i < Array.length instrs then instrs.(i) else Instr.to_int Instr.Nop)
  in
  let ichan = Chan.create CInt.dtype_8 in
  let ochan = Chan.create CInt.dtype_8 in
  let ir = cpu my_instrs ~ichan:ichan.r ~ochan:ochan.w in
  let ichan, ochan = (ichan.w, ochan.r) in
  let sim =
    create ir ~user_sendable_ports:[ ichan.u ] ~user_readable_ports:[ ochan.u ]
  in
  (sim, ichan, ochan)

let push_first v =
  CInt.bit_or (Instr.to_int Push_imm) (CInt.of_int (Int.bit_and v 0xf0))

let push_second v =
  CInt.bit_or (Instr.to_int Push_imm2) (CInt.of_int (Int.bit_and v 0x0f * 16))

let%expect_test "test" =
  let instrs =
    [| push_first 3; push_second 3; Instr.to_int Output; Instr.to_int End |]
  in
  let sim, _, o =
    test instrs ~create:(fun ir ~user_sendable_ports ~user_readable_ports ->
        Sim.simulate_chp ir ~user_sendable_ports ~user_readable_ports)
  in
  Sim.read sim o (CInt.of_int 3);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
      (Ok ()) |}]

let%expect_test "test" =
  let instrs =
    [|
      Instr.to_int Input;
      Instr.to_int Output;
      push_first 0;
      push_second 0;
      push_first 0;
      push_second 0;
      Instr.to_int Jump;
    |]
  in
  let sim, i, o =
    test instrs ~create:(fun ir ~user_sendable_ports ~user_readable_ports ->
        Sim.simulate_chp ir ~user_sendable_ports ~user_readable_ports)
  in
  Sim.send sim i (CInt.of_int 3);
  Sim.read sim o (CInt.of_int 3);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 7);
  Sim.send sim i (CInt.of_int 12);
  Sim.read sim o (CInt.of_int 7);
  Sim.read sim o (CInt.of_int 12);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ()) |}]

let%expect_test "fibonacci" =
  let instrs =
    [|
      (* push return adress *)
      push_first 54;
      push_second 54;
      push_first 0;
      push_second 0;
      (* [TOP; return_addr_high; return_addr_low]; *)
      Instr.to_int Input;
      (* [TOP; n; return_addr_high; return_addr_low]; *)
      (* just slide into the function code! *)

      (* BEGIN FUNCTION - fib *)
      (* Input: [TOP; n; return_addr_high; return_addr_low]; Output: [TOP;
         fib(n)] and pc = return_addr *)
      Instr.to_int Dup;
      Instr.to_int Dup;
      (* [TOP; n; n; n; return_addr_high; return_addr_low]; *)
      push_first 1;
      push_second 1;
      Instr.to_int Eq;
      (* [TOP; n == 1; n; n; return_addr_high; return_addr_low]; *)
      (* 10: *)
      Instr.to_int Exch;
      (* [TOP; n; n == 1; n; return_addr_high; return_addr_low]; *)
      push_first 0;
      push_second 0;
      Instr.to_int Eq;
      (* [TOP; n == 0; n == 1; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Bool_or;
      Instr.to_int Bool_not;
      (* [TOP; !(n == 1 || n == 0); n; return_addr_high; return_addr_low]; *)

      (* skip all the rest of the code if n = 1 *)
      push_first 52;
      push_second 52;
      push_first 0;
      push_second 0;
      (* 20: *) Instr.to_int JumpIfNot;
      (* [TOP; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Dup;
      push_first 1;
      push_second 1;
      Instr.to_int Sub;
      (* [TOP; n-1; n; return_addr_high; return_addr_low]; *)
      push_first 36;
      push_second 36;
      Instr.to_int Exch;
      push_first 0;
      push_second 0;
      (* 30: *) Instr.to_int Exch;
      (* [TOP; n-1; rec1_return_addr_high; rec1_return_addr_low; n;
         return_addr_high; return_addr_low]; *)
      (* push the start of the function *)
      push_first 5;
      push_second 5;
      push_first 0;
      push_second 0;
      (* [TOP; func_start_high; func_start_low; n-1; rec1_return_addr_high;
         rec1_return_addr_low; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Jump;
      (* [TOP; fib(n-1); n; return_addr_high; return_addr_low]; *)

      (* [TOP; fib(n-1); n; return_addr_high; return_addr_low]; *)
      Instr.to_int Exch;
      push_first 2;
      push_second 2;
      Instr.to_int Sub;
      (* [TOP; n-2; fib(n-1);  return_addr_high; return_addr_low]; *)
      (* 40: *)
      push_first 51;
      push_second 51;
      Instr.to_int Exch;
      push_first 0;
      push_second 0;
      Instr.to_int Exch;
      (* [TOP; n-2; rec1_return_addr_high; rec1_return_addr_low; fib(n-1);
         return_addr_high; return_addr_low]; *)
      (* push the start of the function *)
      push_first 5;
      push_second 5;
      push_first 0;
      push_second 0;
      (* [TOP; func_start_high; func_start_low; n-2; rec1_return_addr_high;
         rec1_return_addr_low; fib(n-1); return_addr_high; return_addr_low]; *)
      (* 50: *)
      Instr.to_int Jump;
      (* [TOP; fib(n-2); fib(n-1); return_addr_high; return_addr_low]; *)
      Instr.to_int Add;
      (* [TOP; fib(n); return_addr_high; return_addr_low]; *)

      (* so now in either case it has fib(n) on the top of the stack. So reorder
         the stack and return. *)
      Instr.to_int Exch2;
      (* [TOP; return_addr_high; return_addr_low]; fib(n); *)
      Instr.to_int Jump;
      (* final outputing code *)
      Instr.to_int Output;
      push_first 0;
      push_second 0;
      push_first 0;
      push_second 0;
      Instr.to_int Jump;
    |]
  in
  (* let t = Caml.Sys.time () in *)
  let sim, i, o =
    test instrs ~create:(fun ir ~user_sendable_ports ~user_readable_ports ->
        Sim.simulate_chp ir ~user_sendable_ports ~user_readable_ports)
  in
  Sim.send sim i (CInt.of_int 0);
  Sim.read sim o (CInt.of_int 0);
  print_s [%sexp (Sim.wait sim ~max_steps:100000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 1);
  Sim.read sim o (CInt.of_int 1);
  print_s [%sexp (Sim.wait sim ~max_steps:100000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 2);
  Sim.read sim o (CInt.of_int 1);
  print_s [%sexp (Sim.wait sim ~max_steps:100000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 7);
  Sim.read sim o (CInt.of_int 13);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 8);
  Sim.read sim o (CInt.of_int 21);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 12);
  Sim.read sim o (CInt.of_int 144);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ()) |}];

  (* Printf.printf "Execution time: %fs\n" (Caml.Sys.time () -. t); [%expect {|
     |}]; *)

  (* let t = Caml.Sys.time () in *)
  (* let sim, i, o = test instrs ~create:(fun ir ~user_sendable_ports
     ~user_readable_ports -> Exporter.stf_sim ir ~user_sendable_ports
     ~user_readable_ports) in Sim.send sim i (CInt.of_int 0); Sim.read sim o
     (CInt.of_int 0); print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
     Sim.send sim i (CInt.of_int 1); Sim.read sim o (CInt.of_int 1); print_s
     [%sexp (Sim.wait sim () : unit Or_error.t)]; Sim.send sim i (CInt.of_int
     2); Sim.read sim o (CInt.of_int 1); print_s [%sexp (Sim.wait sim
     ~max_steps:10000 () : unit Or_error.t)]; Sim.send sim i (CInt.of_int 7);
     Sim.read sim o (CInt.of_int 13); print_s [%sexp (Sim.wait sim
     ~max_steps:1000000 () : unit Or_error.t)]; Sim.send sim i (CInt.of_int 8);
     Sim.read sim o (CInt.of_int 21); print_s [%sexp (Sim.wait sim
     ~max_steps:1000000 () : unit Or_error.t)]; Sim.send sim i (CInt.of_int 12);
     Sim.read sim o (CInt.of_int 144); print_s [%sexp (Sim.wait sim
     ~max_steps:1000000 () : unit Or_error.t)]; [%expect {| (Ok ()) (Ok ()) (Ok
     ()) (Ok ()) (Ok ()) (Ok ()) |}]; *)
  (* Printf.printf "Execution time: %fs\n" (Caml.Sys.time () -. t); [%expect {|
     |}]; *)
  let my_instrs =
    Array.init 4096 ~f:(fun i ->
        if i < Array.length instrs then instrs.(i) else Instr.to_int Instr.Nop)
  in
  let ichan = Chan.create CInt.dtype_8 in
  let ochan = Chan.create CInt.dtype_8 in
  let ir = cpu my_instrs ~ichan:ichan.r ~ochan:ochan.w in
  ();

  Compiler.compile_chp ir ~user_sendable_ports:[ ichan.w.u ]
    ~user_readable_ports:[ ochan.r.u ] ~to_:`Dataflow
  |> Compiler.export_print;

  [%expect
    {|
    defproc proc_0(chan!(int<8>) iport225; chan!(int<8>) iport226; chan!(int<8>) iport227; chan?(int<13>) oport206; chan?(int<8>) oport99; chan?(int<13>) oport101; chan?(int<8>) oport214) {
      chan(int<8>) v0;
      chan(int<8>) v1;
      chan(int<8>) v2;
      chan(int<8>) v3;
      chan(int<8>) v4;
      chan(int<8>) v5;
      chan(int<12>) v6;
      chan(int<12>) v7;
      chan(int<1>) v8;
      chan(int<8>) v9;
      chan(int<8>) v10;
      chan(int<8>) v11;
      chan(int<8>) v12;
      chan(int<8>) v13;
      chan(int<8>) v14;
      chan(int<8>) v15;
      chan(int<8>) v16;
      chan(int<1>) v17;
      chan(int<1>) v18;
      chan(int<1>) v19;
      chan(int<12>) v20;
      chan(int<13>) v21;
      chan(int<4>) v22;
      chan(int<4>) v23;
      chan(int<1>) v24;
      chan(int<1>) v25;
      chan(int<2>) v26;
      chan(int<2>) v27;
      chan(int<4>) v28;
      chan(int<4>) v29;
      chan(int<4>) v30;
      chan(int<2>) v31;
      chan(int<2>) v32;
      chan(int<1>) v33;
      chan(int<2>) v34;
      chan(int<1>) v35;
      chan(int<1>) v36;
      chan(int<1>) v37;
      chan(int<12>) v38;
      chan(int<13>) v39;
      chan(int<12>) v40;
      chan(int<13>) v41;
      chan(int<12>) v42;
      chan(int<13>) v43;
      chan(int<1>) v44;
      chan(int<1>) v45;
      chan(int<1>) v46;
      chan(int<8>) v47;
      chan(int<12>) v48;
      chan(int<8>) v49;
      chan(int<8>) v50;
      chan(int<8>) v51;
      chan(int<8>) v52;
      chan(int<8>) v53;
      chan(int<1>) v54;
      chan(int<12>) v55;
      chan(int<12>) v56;
      chan(int<12>) v57;
      chan(int<12>) v58;
      chan(int<12>) v59;
      chan(int<12>) v60;
      chan(int<12>) v61;
      chan(int<12>) v62;
      chan(int<12>) v63;
      chan(int<12>) v64;
      chan(int<12>) v65;
      chan(int<12>) v66;
      chan(int<12>) v67;
      chan(int<12>) v68;
      chan(int<12>) v69;
      chan(int<13>) v70;
      chan(int<12>) v71;
      chan(int<13>) v72;
      chan(int<12>) v73;
      chan(int<13>) v74;
      chan(int<12>) v75;
      chan(int<2>) v76;
      chan(int<2>) v77;
      chan(int<2>) v78;
      chan(int<2>) v79;
      chan(int<3>) v80;
      chan(int<3>) v81;
      chan(int<1>) v82;
      chan(int<1>) v83;
      chan(int<1>) v84;
      chan(int<12>) v85;
      chan(int<1>) v86;
      chan(int<12>) v87;
      chan(int<1>) v88;
      chan(int<1>) v89;
      chan(int<1>) v90;
      chan(int<12>) v91;
      chan(int<1>) v92;
      chan(int<12>) v93;
      chan(int<1>) v94;
      chan(int<8>) v95;
      chan(int<8>) v96;
      chan(int<8>) v97;
      chan(int<2>) v98;
      chan(int<8>) v99;
      chan(int<2>) v100;
      chan(int<13>) v101;
      chan(int<3>) v102;
      chan(int<8>) v103;
      chan(int<12>) v104;
      chan(int<12>) v105;
      chan(int<1>) v106;
      chan(int<8>) v107;
      chan(int<1>) v108;
      chan(int<12>) v109;
      chan(int<1>) v110;
      chan(int<12>) v111;
      chan(int<12>) v112;
      chan(int<1>) v113;
      chan(int<8>) v114;
      chan(int<1>) v115;
      chan(int<12>) v116;
      chan(int<1>) v117;
      chan(int<12>) v118;
      chan(int<12>) v119;
      chan(int<1>) v120;
      chan(int<8>) v121;
      chan(int<1>) v122;
      chan(int<12>) v123;
      chan(int<1>) v124;
      chan(int<8>) v125;
      chan(int<8>) v126;
      chan(int<8>) v127;
      chan(int<8>) v128;
      chan(int<8>) v129;
      chan(int<8>) v130;
      chan(int<8>) v131;
      chan(int<8>) v132;
      chan(int<8>) v133;
      chan(int<8>) v134;
      chan(int<8>) v135;
      chan(int<12>) v136;
      chan(int<12>) v137;
      chan(int<12>) v138;
      chan(int<12>) v139;
      chan(int<12>) v140;
      chan(int<12>) v141;
      chan(int<12>) v142;
      chan(int<12>) v143;
      chan(int<12>) v144;
      chan(int<12>) v145;
      chan(int<12>) v146;
      chan(int<12>) v147;
      chan(int<12>) v148;
      chan(int<12>) v149;
      chan(int<12>) v150;
      chan(int<12>) v151;
      chan(int<4>) v152;
      chan(int<4>) v153;
      chan(int<4>) v154;
      chan(int<8>) v155;
      chan(int<8>) v156;
      chan(int<8>) v157;
      chan(int<12>) v158;
      chan(int<4>) v159;
      chan(int<8>) v160;
      chan(int<1>) v161;
      chan(int<8>) v162;
      chan(int<1>) v163;
      chan(int<12>) v164;
      chan(int<1>) v165;
      chan(int<12>) v166;
      chan(int<1>) v167;
      chan(int<8>) v168;
      chan(int<1>) v169;
      chan(int<12>) v170;
      chan(int<12>) v171;
      chan(int<1>) v172;
      chan(int<12>) v173;
      chan(int<1>) v174;
      chan(int<8>) v175;
      chan(int<1>) v176;
      chan(int<12>) v177;
      chan(int<12>) v178;
      chan(int<1>) v179;
      chan(int<12>) v180;
      chan(int<1>) v181;
      chan(int<8>) v182;
      chan(int<1>) v183;
      chan(int<12>) v184;
      chan(int<12>) v185;
      chan(int<1>) v186;
      chan(int<12>) v187;
      chan(int<1>) v188;
      chan(int<1>) v189;
      chan(int<1>) v190;
      chan(int<8>) v191;
      chan(int<8>) v192;
      chan(int<8>) v193;
      chan(int<8>) v194;
      chan(int<8>) v195;
      chan(int<8>) v196;
      chan(int<8>) v197;
      chan(int<8>) v198;
      chan(int<8>) v199;
      chan(int<8>) v200;
      chan(int<8>) v201;
      chan(int<8>) v202;
      chan(int<8>) v203;
      chan(int<1>) v204;
      chan(int<1>) v205;
      chan(int<13>) v206;
      chan(int<8>) v207;
      chan(int<13>) v208;
      chan(int<8>) v209;
      chan(int<13>) v210;
      chan(int<8>) v211;
      chan(int<13>) v212;
      chan(int<8>) v213;
      chan(int<8>) v214;
      chan(int<8>) v215;
      chan(int<13>) v216;
      chan(int<8>) v217;
      chan(int<13>) v218;
      chan(int<8>) v219;
      chan(int<13>) v220;
      chan(int<8>) v221;
      chan(int<2>) v222;
      chan(int<2>) v223;
      chan(int<3>) v224;
      chan(int<8>) v225;
      chan(int<8>) v226;
      chan(int<8>) v227;
    dataflow {
      v0 <- 0;
      v1 <- 0;
      v2 <- 0;
      v3 <- 0;
      v4 <- 0;
      v5 <- 0;
      v6 <- 0;
      v7 <- 0;
      v8 <- 0;
      v9 <- 0;
      v10 <- 1;
      v11 <- 2;
      v12 <- 3;
      v13 <- 0;
      v14 <- 1;
      v15 <- 2;
      v16 <- 3;
      v17 <- 1;
      v19 <- int((v18) = 0);
      v21 <- ((v20) << 1);
      dataflow_cluser {
        v22 <- int(((v207) >> 4), 4);
        v23 <- ( log2_one_hot({ int(int(15 = (15 & (v207))), 1), int(int(14 = (15 & (v207))), 1), int(int(13 = (15 & (v207))), 1), int(int(12 = (15 & (v207))), 1), int(int(11 = (15 & (v207))), 1), int(int(10 = (15 & (v207))), 1), int(int(9 = (15 & (v207))), 1), int(int(8 = (15 & (v207))), 1), int(int(7 = (15 & (v207))), 1), int(int(6 = (15 & (v207))), 1), int(int(5 = (15 & (v207))), 1), int(int(4 = (15 & (v207))), 1), int(int(3 = (15 & (v207))), 1), int(int(2 = (15 & (v207))), 1), int(int(1 = (15 & (v207))), 1), int(int(0 = (15 & (v207))), 1) }) );
        v24 <- ( log2_one_hot({ int(((((((((((((((int(1 = (15 & (v207))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(5 = (15 & (v207)))) | int(6 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(int(0 = (15 & (v207))), 1) }) );
        v25 <- ( log2_one_hot({ int(int(0 = (15 & (v207))), 1), int(((((((((((((((int(1 = (15 & (v207))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(5 = (15 & (v207)))) | int(6 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1) }) );
        v26 <- ( log2_one_hot({ int((((((((((((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(5 = (15 & (v207)))) | int(6 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(int(3 = (15 & (v207))), 1), int(int(2 = (15 & (v207))), 1) }) );
        v27 <- ( log2_one_hot({ int(((((((((((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(int(6 = (15 & (v207))), 1), int(int(5 = (15 & (v207))), 1), int(int(4 = (15 & (v207))), 1) }) );
        v28 <- ( log2_one_hot({ int(((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(12 = (15 & (v207)))), 1), int((int(5 = (15 & (v207))) | int(6 = (15 & (v207)))), 1), int(int(15 = (15 & (v207))), 1), int(int(14 = (15 & (v207))), 1), int(int(13 = (15 & (v207))), 1), int(int(11 = (15 & (v207))), 1), int(int(10 = (15 & (v207))), 1), int(int(9 = (15 & (v207))), 1), int(int(4 = (15 & (v207))), 1), int(int(3 = (15 & (v207))), 1), int(int(2 = (15 & (v207))), 1) }) );
        v29 <- ( log2_one_hot({ int((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(13 = (15 & (v207)))), 1), int((int(5 = (15 & (v207))) | int(6 = (15 & (v207)))), 1), int(int(15 = (15 & (v207))), 1), int(int(14 = (15 & (v207))), 1), int(int(12 = (15 & (v207))), 1), int(int(11 = (15 & (v207))), 1), int(int(10 = (15 & (v207))), 1), int(int(9 = (15 & (v207))), 1), int(int(8 = (15 & (v207))), 1), int(int(7 = (15 & (v207))), 1), int(int(4 = (15 & (v207))), 1), int(int(3 = (15 & (v207))), 1) }) );
        v30 <- ( log2_one_hot({ int((((((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))), 1), int(int(15 = (15 & (v207))), 1), int(int(11 = (15 & (v207))), 1), int(int(10 = (15 & (v207))), 1), int(int(9 = (15 & (v207))), 1), int(int(8 = (15 & (v207))), 1), int(int(7 = (15 & (v207))), 1), int(int(6 = (15 & (v207))), 1), int(int(5 = (15 & (v207))), 1) }) );
        v31 <- ( log2_one_hot({ int((((((((((((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(5 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(int(8 = (15 & (v207))), 1), int(int(6 = (15 & (v207))), 1) }) );
        v32 <- ( log2_one_hot({ int(int(6 = (15 & (v207))), 1), int((int(4 = (15 & (v207))) | int(5 = (15 & (v207)))), 1), int((((((((int(2 = (15 & (v207))) | int(3 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(12 = (15 & (v207)))), 1) }) );
        v33 <- ( log2_one_hot({ int(((((((((((((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(3 = (15 & (v207)))) | int(4 = (15 & (v207)))) | int(5 = (15 & (v207)))) | int(7 = (15 & (v207)))) | int(8 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(13 = (15 & (v207)))) | int(14 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int(int(6 = (15 & (v207))), 1) }) );
        v34 <- ( log2_one_hot({ int((int(6 = (15 & (v207))) | int(8 = (15 & (v207)))), 1), int((((((int(5 = (15 & (v207))) | int(7 = (15 & (v207)))) | int(9 = (15 & (v207)))) | int(10 = (15 & (v207)))) | int(11 = (15 & (v207)))) | int(15 = (15 & (v207)))), 1), int((((int(3 = (15 & (v207))) | int(4 = (15 & (v207)))) | int(12 = (15 & (v207)))) | int(14 = (15 & (v207)))), 1), int((((int(0 = (15 & (v207))) | int(1 = (15 & (v207)))) | int(2 = (15 & (v207)))) | int(13 = (15 & (v207)))), 1) }) );
      };
      dataflow_cluser {
        v35 <- ( log2_one_hot({ int(int(int((v103) >= 1) = 0), 1), int(int((v103) >= 1), 1) }) );
        v36 <- ( log2_one_hot({ int(int(int((v103) >= 2) = 0), 1), int(int((v103) >= 2), 1) }) );
        v37 <- ( log2_one_hot({ int(int(int((v103) >= 3) = 0), 1), int(int((v103) >= 3), 1) }) );
      };
      dataflow_cluser {
        v38 <- ((v104) + 1);
        v39 <- (((v104) + 1) << 1);
      };
      dataflow_cluser {
        v40 <- ((v111) + 1);
        v41 <- (((v111) + 1) << 1);
      };
      dataflow_cluser {
        v42 <- ((v118) + 1);
        v43 <- (((v118) + 1) << 1);
      };
      dataflow_cluser {
        v44 <- ( log2_one_hot({ int(int(int((v125) >= 1) = 0), 1), int(int((v125) >= 1), 1) }) );
        v45 <- ( log2_one_hot({ int(int(int((v125) >= 2) = 0), 1), int(int((v125) >= 2), 1) }) );
        v46 <- ( log2_one_hot({ int(int(int((v125) >= 3) = 0), 1), int(int((v125) >= 3), 1) }) );
      };
      v47 <- ((v154) | int((int((v191), 4) << 4), 8));
      v48 <- int(((v128) | ((v193) << 8)), 12);
      v49 <- int((v195) = (v130));
      v50 <- int(((v196) + (v131)), 8);
      v51 <- int(((int((v132), 8) | 256) + int((v197), 8)), 8);
      v52 <- int(0 = (v199));
      v53 <- (int(0 != (v200)) | int(0 != (v133)));
      v54 <- ( log2_one_hot({ int(int(1 = int((v135) = 0)), 1), int(int(0 = int((v135) = 0)), 1) }) );
      v55 <- (1 + (v137));
      v56 <- (1 + (v138));
      v57 <- (1 + (v139));
      v58 <- (1 + (v140));
      v59 <- (1 + (v141));
      v60 <- (1 + (v142));
      v61 <- (1 + (v144));
      v62 <- (1 + (v145));
      v63 <- (1 + (v146));
      v64 <- (1 + (v147));
      v65 <- (1 + (v148));
      v66 <- (1 + (v149));
      v67 <- (1 + (v150));
      v68 <- int(((v162) | ((v160) << 8)), 12);
      v69 <- (1 + (v164));
      dataflow_cluser {
        v70 <- { int((v170), 12), int(1, 1) };
        v71 <- (1 + (v170));
      };
      dataflow_cluser {
        v72 <- { int((v177), 12), int(1, 1) };
        v73 <- (1 + (v177));
      };
      dataflow_cluser {
        v74 <- { int((v184), 12), int(1, 1) };
        v75 <- (1 + (v184));
      };
      v77 <- (((v76) + 1) + (3 * int((v76) = 3)));
      v79 <- (((v78) + 1) + (3 * int((v78) = 3)));
      v81 <- (((v80) + 1) + (6 * int((v80) = 6)));
      { v84 } v83 -> *, v82;
      { v86 } v158 -> *, v85;
      { v88 } v187 -> *, v87;
      { v90 } v8, v82 -> v89;
      { v92 } v6, v85 -> v91;
      { v94 } v7, v87 -> v93;
      { v98 } v226 -> v95, v96, v97;
      { v100 } v217, v219, v221 -> v99;
      { v102 } v208, v210, v212, v216, v218, v220 -> v101;
      { v34 } v9, v10, v11, v12 -> v103;
      { v106 } v93 -> v104, v105;
      { v108 } v209, v0 -> v107;
      { v110 } v38, v105 -> v109;
      { v113 } v109 -> v111, v112;
      { v115 } v211, v1 -> v114;
      { v117 } v40, v112 -> v116;
      { v120 } v116 -> v118, v119;
      { v122 } v213, v2 -> v121;
      { v124 } v42, v119 -> v123;
      { v32 } v13, v14, v15, v16 -> v125;
      { v30 } v114 -> v126, v127, v128, v129, v130, v131, v132, v133, *;
      { v31 } v121 -> v134, v135, *;
      { v152 } v151 -> v136, v137, v138, v139, v140, v141, v142, *, v143, v144, v145, v146, v147, v148, v149, v150;
      { v26 } v22 -> v153, v154, *;
      { v27 } v156, v126, v134, v4 -> v155;
      { v33 } v127, v5 -> v157;
      { v159 } v136, v55, v56, v57, v58, v59, v60, v48, v166, v61, v62, v63, v64, v65, v66, v67 -> v158;
      { v161 } v194 -> *, v160;
      { v163 } v129 -> *, v162;
      { v165 } v143 -> v164, *;
      { v167 } v69, v68 -> v166;
      { v169 } v202 -> v168, *;
      { v172 } v123 -> v170, v171;
      { v174 } v71, v171 -> v173;
      { v176 } v155 -> v175, *;
      { v179 } v173 -> v177, v178;
      { v181 } v73, v178 -> v180;
      { v183 } v157 -> v182, *;
      { v186 } v180 -> v184, v185;
      { v188 } v75, v185 -> v187;
      { v24 } v89 -> *, v189;
      { v25 } v189, v17 -> v190;
      { v29 } v107 -> v191, v192, v193, v194, v195, v196, v197, v198, v199, v200, v201, *;
      { v28 } v153, v47, v203, v49, v50, v51, v215, v52, v53, v201, v3 -> v202;
      v204 -> [1,0] v205;
      v21 -> [1] v206;
      v225 -> [1] v207;
      v39 -> [1] v208;
      v95 -> [1] v209;
      v41 -> [1] v210;
      v96 -> [1] v211;
      v43 -> [1] v212;
      v97 -> [1] v213;
      v198 -> [1] v214;
      v227 -> [1] v215;
      v70 -> [1] v216;
      v168 -> [1] v217;
      v72 -> [1] v218;
      v175 -> [1] v219;
      v74 -> [1] v220;
      v182 -> [1] v221;
      v77 -> [1,0] v222;
      v79 -> [1,0] v223;
      v81 -> [1,0] v224;
      v204 <- v19;     v88 <- v19;     v86 <- v19;     v84 <- v19;
      v159 <- v23;     v152 <- v23;
      v110 <- v35;     v108 <- v35;     v106 <- v35;
      v117 <- v36;     v115 <- v36;     v113 <- v36;
      v124 <- v37;     v122 <- v37;     v120 <- v37;
      v174 <- v44;     v172 <- v44;     v169 <- v44;
      v181 <- v45;     v179 <- v45;     v176 <- v45;
      v188 <- v46;     v186 <- v46;     v183 <- v46;
      v167 <- v54;     v165 <- v54;     v163 <- v54;     v161 <- v54;
      v151 <- v91;     v20 <- v91;
      v83 <- v190;     v18 <- v190;
      v203 <- v192;     v156 <- v192;
      v94 <- v205;     v92 <- v205;     v90 <- v205;
      v98 <- v222;     v76 <- v222;
      v100 <- v223;     v78 <- v223;
      v102 <- v224;     v80 <- v224;
    iport225 -> v225;
    iport226 -> v226;
    iport227 -> v227;
    v206 -> oport206;
    v99 -> oport99;
    v101 -> oport101;
    v214 -> oport214;
    }
    }

    defproc proc_1(chan?(int<13>>) cmd_chan; chan?(int<8>>) read_chan) {

    int<8> v[4096];
    int<13> cmd;
    int<13> tmp;
    chp {
    v[0] := 50;v[1] := 99;v[2] := 2;v[3] := 3;v[4] := 13;v[5] := 4;v[6] := 4;v[7] := 2;v[8] := 19;v[9] := 9;v[10] := 5;v[11] := 2;v[12] := 3;v[13] := 9;v[14] := 15;v[15] := 14;v[16] := 50;v[17] := 67;v[18] := 2;v[19] := 3;v[20] := 8;v[21] := 4;v[22] := 2;v[23] := 19;v[24] := 11;v[25] := 34;v[26] := 67;v[27] := 5;v[28] := 2;v[29] := 3;v[30] := 5;v[31] := 2;v[32] := 83;v[33] := 2;v[34] := 3;v[35] := 7;v[36] := 5;v[37] := 2;v[38] := 35;v[39] := 11;v[40] := 50;v[41] := 51;v[42] := 5;v[43] := 2;v[44] := 3;v[45] := 5;v[46] := 2;v[47] := 83;v[48] := 2;v[49] := 3;v[50] := 7;v[51] := 10;v[52] := 6;v[53] := 7;v[54] := 12;v[55] := 2;v[56] := 3;v[57] := 2;v[58] := 3;v[59] := 7;v[60] := 1;v[61] := 1;v[62] := 1;v[63] := 1;v[64] := 1;v[65] := 1;v[66] := 1;v[67] := 1;v[68] := 1;v[69] := 1;v[70] := 1;v[71] := 1;v[72] := 1;v[73] := 1;v[74] := 1;v[75] := 1;v[76] := 1;v[77] := 1;v[78] := 1;v[79] := 1;v[80] := 1;v[81] := 1;v[82] := 1;v[83] := 1;v[84] := 1;v[85] := 1;v[86] := 1;v[87] := 1;v[88] := 1;v[89] := 1;v[90] := 1;v[91] := 1;v[92] := 1;v[93] := 1;v[94] := 1;v[95] := 1;v[96] := 1;v[97] := 1;v[98] := 1;v[99] := 1;v[100] := 1;v[101] := 1;v[102] := 1;v[103] := 1;v[104] := 1;v[105] := 1;v[106] := 1;v[107] := 1;v[108] := 1;v[109] := 1;v[110] := 1;v[111] := 1;v[112] := 1;v[113] := 1;v[114] := 1;v[115] := 1;v[116] := 1;v[117] := 1;v[118] := 1;v[119] := 1;v[120] := 1;v[121] := 1;v[122] := 1;v[123] := 1;v[124] := 1;v[125] := 1;v[126] := 1;v[127] := 1;v[128] := 1;v[129] := 1;v[130] := 1;v[131] := 1;v[132] := 1;v[133] := 1;v[134] := 1;v[135] := 1;v[136] := 1;v[137] := 1;v[138] := 1;v[139] := 1;v[140] := 1;v[141] := 1;v[142] := 1;v[143] := 1;v[144] := 1;v[145] := 1;v[146] := 1;v[147] := 1;v[148] := 1;v[149] := 1;v[150] := 1;v[151] := 1;v[152] := 1;v[153] := 1;v[154] := 1;v[155] := 1;v[156] := 1;v[157] := 1;v[158] := 1;v[159] := 1;v[160] := 1;v[161] := 1;v[162] := 1;v[163] := 1;v[164] := 1;v[165] := 1;v[166] := 1;v[167] := 1;v[168] := 1;v[169] := 1;v[170] := 1;v[171] := 1;v[172] := 1;v[173] := 1;v[174] := 1;v[175] := 1;v[176] := 1;v[177] := 1;v[178] := 1;v[179] := 1;v[180] := 1;v[181] := 1;v[182] := 1;v[183] := 1;v[184] := 1;v[185] := 1;v[186] := 1;v[187] := 1;v[188] := 1;v[189] := 1;v[190] := 1;v[191] := 1;v[192] := 1;v[193] := 1;v[194] := 1;v[195] := 1;v[196] := 1;v[197] := 1;v[198] := 1;v[199] := 1;v[200] := 1;v[201] := 1;v[202] := 1;v[203] := 1;v[204] := 1;v[205] := 1;v[206] := 1;v[207] := 1;v[208] := 1;v[209] := 1;v[210] := 1;v[211] := 1;v[212] := 1;v[213] := 1;v[214] := 1;v[215] := 1;v[216] := 1;v[217] := 1;v[218] := 1;v[219] := 1;v[220] := 1;v[221] := 1;v[222] := 1;v[223] := 1;v[224] := 1;v[225] := 1;v[226] := 1;v[227] := 1;v[228] := 1;v[229] := 1;v[230] := 1;v[231] := 1;v[232] := 1;v[233] := 1;v[234] := 1;v[235] := 1;v[236] := 1;v[237] := 1;v[238] := 1;v[239] := 1;v[240] := 1;v[241] := 1;v[242] := 1;v[243] := 1;v[244] := 1;v[245] := 1;v[246] := 1;v[247] := 1;v[248] := 1;v[249] := 1;v[250] := 1;v[251] := 1;v[252] := 1;v[253] := 1;v[254] := 1;v[255] := 1;v[256] := 1;v[257] := 1;v[258] := 1;v[259] := 1;v[260] := 1;v[261] := 1;v[262] := 1;v[263] := 1;v[264] := 1;v[265] := 1;v[266] := 1;v[267] := 1;v[268] := 1;v[269] := 1;v[270] := 1;v[271] := 1;v[272] := 1;v[273] := 1;v[274] := 1;v[275] := 1;v[276] := 1;v[277] := 1;v[278] := 1;v[279] := 1;v[280] := 1;v[281] := 1;v[282] := 1;v[283] := 1;v[284] := 1;v[285] := 1;v[286] := 1;v[287] := 1;v[288] := 1;v[289] := 1;v[290] := 1;v[291] := 1;v[292] := 1;v[293] := 1;v[294] := 1;v[295] := 1;v[296] := 1;v[297] := 1;v[298] := 1;v[299] := 1;v[300] := 1;v[301] := 1;v[302] := 1;v[303] := 1;v[304] := 1;v[305] := 1;v[306] := 1;v[307] := 1;v[308] := 1;v[309] := 1;v[310] := 1;v[311] := 1;v[312] := 1;v[313] := 1;v[314] := 1;v[315] := 1;v[316] := 1;v[317] := 1;v[318] := 1;v[319] := 1;v[320] := 1;v[321] := 1;v[322] := 1;v[323] := 1;v[324] := 1;v[325] := 1;v[326] := 1;v[327] := 1;v[328] := 1;v[329] := 1;v[330] := 1;v[331] := 1;v[332] := 1;v[333] := 1;v[334] := 1;v[335] := 1;v[336] := 1;v[337] := 1;v[338] := 1;v[339] := 1;v[340] := 1;v[341] := 1;v[342] := 1;v[343] := 1;v[344] := 1;v[345] := 1;v[346] := 1;v[347] := 1;v[348] := 1;v[349] := 1;v[350] := 1;v[351] := 1;v[352] := 1;v[353] := 1;v[354] := 1;v[355] := 1;v[356] := 1;v[357] := 1;v[358] := 1;v[359] := 1;v[360] := 1;v[361] := 1;v[362] := 1;v[363] := 1;v[364] := 1;v[365] := 1;v[366] := 1;v[367] := 1;v[368] := 1;v[369] := 1;v[370] := 1;v[371] := 1;v[372] := 1;v[373] := 1;v[374] := 1;v[375] := 1;v[376] := 1;v[377] := 1;v[378] := 1;v[379] := 1;v[380] := 1;v[381] := 1;v[382] := 1;v[383] := 1;v[384] := 1;v[385] := 1;v[386] := 1;v[387] := 1;v[388] := 1;v[389] := 1;v[390] := 1;v[391] := 1;v[392] := 1;v[393] := 1;v[394] := 1;v[395] := 1;v[396] := 1;v[397] := 1;v[398] := 1;v[399] := 1;v[400] := 1;v[401] := 1;v[402] := 1;v[403] := 1;v[404] := 1;v[405] := 1;v[406] := 1;v[407] := 1;v[408] := 1;v[409] := 1;v[410] := 1;v[411] := 1;v[412] := 1;v[413] := 1;v[414] := 1;v[415] := 1;v[416] := 1;v[417] := 1;v[418] := 1;v[419] := 1;v[420] := 1;v[421] := 1;v[422] := 1;v[423] := 1;v[424] := 1;v[425] := 1;v[426] := 1;v[427] := 1;v[428] := 1;v[429] := 1;v[430] := 1;v[431] := 1;v[432] := 1;v[433] := 1;v[434] := 1;v[435] := 1;v[436] := 1;v[437] := 1;v[438] := 1;v[439] := 1;v[440] := 1;v[441] := 1;v[442] := 1;v[443] := 1;v[444] := 1;v[445] := 1;v[446] := 1;v[447] := 1;v[448] := 1;v[449] := 1;v[450] := 1;v[451] := 1;v[452] := 1;v[453] := 1;v[454] := 1;v[455] := 1;v[456] := 1;v[457] := 1;v[458] := 1;v[459] := 1;v[460] := 1;v[461] := 1;v[462] := 1;v[463] := 1;v[464] := 1;v[465] := 1;v[466] := 1;v[467] := 1;v[468] := 1;v[469] := 1;v[470] := 1;v[471] := 1;v[472] := 1;v[473] := 1;v[474] := 1;v[475] := 1;v[476] := 1;v[477] := 1;v[478] := 1;v[479] := 1;v[480] := 1;v[481] := 1;v[482] := 1;v[483] := 1;v[484] := 1;v[485] := 1;v[486] := 1;v[487] := 1;v[488] := 1;v[489] := 1;v[490] := 1;v[491] := 1;v[492] := 1;v[493] := 1;v[494] := 1;v[495] := 1;v[496] := 1;v[497] := 1;v[498] := 1;v[499] := 1;v[500] := 1;v[501] := 1;v[502] := 1;v[503] := 1;v[504] := 1;v[505] := 1;v[506] := 1;v[507] := 1;v[508] := 1;v[509] := 1;v[510] := 1;v[511] := 1;v[512] := 1;v[513] := 1;v[514] := 1;v[515] := 1;v[516] := 1;v[517] := 1;v[518] := 1;v[519] := 1;v[520] := 1;v[521] := 1;v[522] := 1;v[523] := 1;v[524] := 1;v[525] := 1;v[526] := 1;v[527] := 1;v[528] := 1;v[529] := 1;v[530] := 1;v[531] := 1;v[532] := 1;v[533] := 1;v[534] := 1;v[535] := 1;v[536] := 1;v[537] := 1;v[538] := 1;v[539] := 1;v[540] := 1;v[541] := 1;v[542] := 1;v[543] := 1;v[544] := 1;v[545] := 1;v[546] := 1;v[547] := 1;v[548] := 1;v[549] := 1;v[550] := 1;v[551] := 1;v[552] := 1;v[553] := 1;v[554] := 1;v[555] := 1;v[556] := 1;v[557] := 1;v[558] := 1;v[559] := 1;v[560] := 1;v[561] := 1;v[562] := 1;v[563] := 1;v[564] := 1;v[565] := 1;v[566] := 1;v[567] := 1;v[568] := 1;v[569] := 1;v[570] := 1;v[571] := 1;v[572] := 1;v[573] := 1;v[574] := 1;v[575] := 1;v[576] := 1;v[577] := 1;v[578] := 1;v[579] := 1;v[580] := 1;v[581] := 1;v[582] := 1;v[583] := 1;v[584] := 1;v[585] := 1;v[586] := 1;v[587] := 1;v[588] := 1;v[589] := 1;v[590] := 1;v[591] := 1;v[592] := 1;v[593] := 1;v[594] := 1;v[595] := 1;v[596] := 1;v[597] := 1;v[598] := 1;v[599] := 1;v[600] := 1;v[601] := 1;v[602] := 1;v[603] := 1;v[604] := 1;v[605] := 1;v[606] := 1;v[607] := 1;v[608] := 1;v[609] := 1;v[610] := 1;v[611] := 1;v[612] := 1;v[613] := 1;v[614] := 1;v[615] := 1;v[616] := 1;v[617] := 1;v[618] := 1;v[619] := 1;v[620] := 1;v[621] := 1;v[622] := 1;v[623] := 1;v[624] := 1;v[625] := 1;v[626] := 1;v[627] := 1;v[628] := 1;v[629] := 1;v[630] := 1;v[631] := 1;v[632] := 1;v[633] := 1;v[634] := 1;v[635] := 1;v[636] := 1;v[637] := 1;v[638] := 1;v[639] := 1;v[640] := 1;v[641] := 1;v[642] := 1;v[643] := 1;v[644] := 1;v[645] := 1;v[646] := 1;v[647] := 1;v[648] := 1;v[649] := 1;v[650] := 1;v[651] := 1;v[652] := 1;v[653] := 1;v[654] := 1;v[655] := 1;v[656] := 1;v[657] := 1;v[658] := 1;v[659] := 1;v[660] := 1;v[661] := 1;v[662] := 1;v[663] := 1;v[664] := 1;v[665] := 1;v[666] := 1;v[667] := 1;v[668] := 1;v[669] := 1;v[670] := 1;v[671] := 1;v[672] := 1;v[673] := 1;v[674] := 1;v[675] := 1;v[676] := 1;v[677] := 1;v[678] := 1;v[679] := 1;v[680] := 1;v[681] := 1;v[682] := 1;v[683] := 1;v[684] := 1;v[685] := 1;v[686] := 1;v[687] := 1;v[688] := 1;v[689] := 1;v[690] := 1;v[691] := 1;v[692] := 1;v[693] := 1;v[694] := 1;v[695] := 1;v[696] := 1;v[697] := 1;v[698] := 1;v[699] := 1;v[700] := 1;v[701] := 1;v[702] := 1;v[703] := 1;v[704] := 1;v[705] := 1;v[706] := 1;v[707] := 1;v[708] := 1;v[709] := 1;v[710] := 1;v[711] := 1;v[712] := 1;v[713] := 1;v[714] := 1;v[715] := 1;v[716] := 1;v[717] := 1;v[718] := 1;v[719] := 1;v[720] := 1;v[721] := 1;v[722] := 1;v[723] := 1;v[724] := 1;v[725] := 1;v[726] := 1;v[727] := 1;v[728] := 1;v[729] := 1;v[730] := 1;v[731] := 1;v[732] := 1;v[733] := 1;v[734] := 1;v[735] := 1;v[736] := 1;v[737] := 1;v[738] := 1;v[739] := 1;v[740] := 1;v[741] := 1;v[742] := 1;v[743] := 1;v[744] := 1;v[745] := 1;v[746] := 1;v[747] := 1;v[748] := 1;v[749] := 1;v[750] := 1;v[751] := 1;v[752] := 1;v[753] := 1;v[754] := 1;v[755] := 1;v[756] := 1;v[757] := 1;v[758] := 1;v[759] := 1;v[760] := 1;v[761] := 1;v[762] := 1;v[763] := 1;v[764] := 1;v[765] := 1;v[766] := 1;v[767] := 1;v[768] := 1;v[769] := 1;v[770] := 1;v[771] := 1;v[772] := 1;v[773] := 1;v[774] := 1;v[775] := 1;v[776] := 1;v[777] := 1;v[778] := 1;v[779] := 1;v[780] := 1;v[781] := 1;v[782] := 1;v[783] := 1;v[784] := 1;v[785] := 1;v[786] := 1;v[787] := 1;v[788] := 1;v[789] := 1;v[790] := 1;v[791] := 1;v[792] := 1;v[793] := 1;v[794] := 1;v[795] := 1;v[796] := 1;v[797] := 1;v[798] := 1;v[799] := 1;v[800] := 1;v[801] := 1;v[802] := 1;v[803] := 1;v[804] := 1;v[805] := 1;v[806] := 1;v[807] := 1;v[808] := 1;v[809] := 1;v[810] := 1;v[811] := 1;v[812] := 1;v[813] := 1;v[814] := 1;v[815] := 1;v[816] := 1;v[817] := 1;v[818] := 1;v[819] := 1;v[820] := 1;v[821] := 1;v[822] := 1;v[823] := 1;v[824] := 1;v[825] := 1;v[826] := 1;v[827] := 1;v[828] := 1;v[829] := 1;v[830] := 1;v[831] := 1;v[832] := 1;v[833] := 1;v[834] := 1;v[835] := 1;v[836] := 1;v[837] := 1;v[838] := 1;v[839] := 1;v[840] := 1;v[841] := 1;v[842] := 1;v[843] := 1;v[844] := 1;v[845] := 1;v[846] := 1;v[847] := 1;v[848] := 1;v[849] := 1;v[850] := 1;v[851] := 1;v[852] := 1;v[853] := 1;v[854] := 1;v[855] := 1;v[856] := 1;v[857] := 1;v[858] := 1;v[859] := 1;v[860] := 1;v[861] := 1;v[862] := 1;v[863] := 1;v[864] := 1;v[865] := 1;v[866] := 1;v[867] := 1;v[868] := 1;v[869] := 1;v[870] := 1;v[871] := 1;v[872] := 1;v[873] := 1;v[874] := 1;v[875] := 1;v[876] := 1;v[877] := 1;v[878] := 1;v[879] := 1;v[880] := 1;v[881] := 1;v[882] := 1;v[883] := 1;v[884] := 1;v[885] := 1;v[886] := 1;v[887] := 1;v[888] := 1;v[889] := 1;v[890] := 1;v[891] := 1;v[892] := 1;v[893] := 1;v[894] := 1;v[895] := 1;v[896] := 1;v[897] := 1;v[898] := 1;v[899] := 1;v[900] := 1;v[901] := 1;v[902] := 1;v[903] := 1;v[904] := 1;v[905] := 1;v[906] := 1;v[907] := 1;v[908] := 1;v[909] := 1;v[910] := 1;v[911] := 1;v[912] := 1;v[913] := 1;v[914] := 1;v[915] := 1;v[916] := 1;v[917] := 1;v[918] := 1;v[919] := 1;v[920] := 1;v[921] := 1;v[922] := 1;v[923] := 1;v[924] := 1;v[925] := 1;v[926] := 1;v[927] := 1;v[928] := 1;v[929] := 1;v[930] := 1;v[931] := 1;v[932] := 1;v[933] := 1;v[934] := 1;v[935] := 1;v[936] := 1;v[937] := 1;v[938] := 1;v[939] := 1;v[940] := 1;v[941] := 1;v[942] := 1;v[943] := 1;v[944] := 1;v[945] := 1;v[946] := 1;v[947] := 1;v[948] := 1;v[949] := 1;v[950] := 1;v[951] := 1;v[952] := 1;v[953] := 1;v[954] := 1;v[955] := 1;v[956] := 1;v[957] := 1;v[958] := 1;v[959] := 1;v[960] := 1;v[961] := 1;v[962] := 1;v[963] := 1;v[964] := 1;v[965] := 1;v[966] := 1;v[967] := 1;v[968] := 1;v[969] := 1;v[970] := 1;v[971] := 1;v[972] := 1;v[973] := 1;v[974] := 1;v[975] := 1;v[976] := 1;v[977] := 1;v[978] := 1;v[979] := 1;v[980] := 1;v[981] := 1;v[982] := 1;v[983] := 1;v[984] := 1;v[985] := 1;v[986] := 1;v[987] := 1;v[988] := 1;v[989] := 1;v[990] := 1;v[991] := 1;v[992] := 1;v[993] := 1;v[994] := 1;v[995] := 1;v[996] := 1;v[997] := 1;v[998] := 1;v[999] := 1;v[1000] := 1;v[1001] := 1;v[1002] := 1;v[1003] := 1;v[1004] := 1;v[1005] := 1;v[1006] := 1;v[1007] := 1;v[1008] := 1;v[1009] := 1;v[1010] := 1;v[1011] := 1;v[1012] := 1;v[1013] := 1;v[1014] := 1;v[1015] := 1;v[1016] := 1;v[1017] := 1;v[1018] := 1;v[1019] := 1;v[1020] := 1;v[1021] := 1;v[1022] := 1;v[1023] := 1;v[1024] := 1;v[1025] := 1;v[1026] := 1;v[1027] := 1;v[1028] := 1;v[1029] := 1;v[1030] := 1;v[1031] := 1;v[1032] := 1;v[1033] := 1;v[1034] := 1;v[1035] := 1;v[1036] := 1;v[1037] := 1;v[1038] := 1;v[1039] := 1;v[1040] := 1;v[1041] := 1;v[1042] := 1;v[1043] := 1;v[1044] := 1;v[1045] := 1;v[1046] := 1;v[1047] := 1;v[1048] := 1;v[1049] := 1;v[1050] := 1;v[1051] := 1;v[1052] := 1;v[1053] := 1;v[1054] := 1;v[1055] := 1;v[1056] := 1;v[1057] := 1;v[1058] := 1;v[1059] := 1;v[1060] := 1;v[1061] := 1;v[1062] := 1;v[1063] := 1;v[1064] := 1;v[1065] := 1;v[1066] := 1;v[1067] := 1;v[1068] := 1;v[1069] := 1;v[1070] := 1;v[1071] := 1;v[1072] := 1;v[1073] := 1;v[1074] := 1;v[1075] := 1;v[1076] := 1;v[1077] := 1;v[1078] := 1;v[1079] := 1;v[1080] := 1;v[1081] := 1;v[1082] := 1;v[1083] := 1;v[1084] := 1;v[1085] := 1;v[1086] := 1;v[1087] := 1;v[1088] := 1;v[1089] := 1;v[1090] := 1;v[1091] := 1;v[1092] := 1;v[1093] := 1;v[1094] := 1;v[1095] := 1;v[1096] := 1;v[1097] := 1;v[1098] := 1;v[1099] := 1;v[1100] := 1;v[1101] := 1;v[1102] := 1;v[1103] := 1;v[1104] := 1;v[1105] := 1;v[1106] := 1;v[1107] := 1;v[1108] := 1;v[1109] := 1;v[1110] := 1;v[1111] := 1;v[1112] := 1;v[1113] := 1;v[1114] := 1;v[1115] := 1;v[1116] := 1;v[1117] := 1;v[1118] := 1;v[1119] := 1;v[1120] := 1;v[1121] := 1;v[1122] := 1;v[1123] := 1;v[1124] := 1;v[1125] := 1;v[1126] := 1;v[1127] := 1;v[1128] := 1;v[1129] := 1;v[1130] := 1;v[1131] := 1;v[1132] := 1;v[1133] := 1;v[1134] := 1;v[1135] := 1;v[1136] := 1;v[1137] := 1;v[1138] := 1;v[1139] := 1;v[1140] := 1;v[1141] := 1;v[1142] := 1;v[1143] := 1;v[1144] := 1;v[1145] := 1;v[1146] := 1;v[1147] := 1;v[1148] := 1;v[1149] := 1;v[1150] := 1;v[1151] := 1;v[1152] := 1;v[1153] := 1;v[1154] := 1;v[1155] := 1;v[1156] := 1;v[1157] := 1;v[1158] := 1;v[1159] := 1;v[1160] := 1;v[1161] := 1;v[1162] := 1;v[1163] := 1;v[1164] := 1;v[1165] := 1;v[1166] := 1;v[1167] := 1;v[1168] := 1;v[1169] := 1;v[1170] := 1;v[1171] := 1;v[1172] := 1;v[1173] := 1;v[1174] := 1;v[1175] := 1;v[1176] := 1;v[1177] := 1;v[1178] := 1;v[1179] := 1;v[1180] := 1;v[1181] := 1;v[1182] := 1;v[1183] := 1;v[1184] := 1;v[1185] := 1;v[1186] := 1;v[1187] := 1;v[1188] := 1;v[1189] := 1;v[1190] := 1;v[1191] := 1;v[1192] := 1;v[1193] := 1;v[1194] := 1;v[1195] := 1;v[1196] := 1;v[1197] := 1;v[1198] := 1;v[1199] := 1;v[1200] := 1;v[1201] := 1;v[1202] := 1;v[1203] := 1;v[1204] := 1;v[1205] := 1;v[1206] := 1;v[1207] := 1;v[1208] := 1;v[1209] := 1;v[1210] := 1;v[1211] := 1;v[1212] := 1;v[1213] := 1;v[1214] := 1;v[1215] := 1;v[1216] := 1;v[1217] := 1;v[1218] := 1;v[1219] := 1;v[1220] := 1;v[1221] := 1;v[1222] := 1;v[1223] := 1;v[1224] := 1;v[1225] := 1;v[1226] := 1;v[1227] := 1;v[1228] := 1;v[1229] := 1;v[1230] := 1;v[1231] := 1;v[1232] := 1;v[1233] := 1;v[1234] := 1;v[1235] := 1;v[1236] := 1;v[1237] := 1;v[1238] := 1;v[1239] := 1;v[1240] := 1;v[1241] := 1;v[1242] := 1;v[1243] := 1;v[1244] := 1;v[1245] := 1;v[1246] := 1;v[1247] := 1;v[1248] := 1;v[1249] := 1;v[1250] := 1;v[1251] := 1;v[1252] := 1;v[1253] := 1;v[1254] := 1;v[1255] := 1;v[1256] := 1;v[1257] := 1;v[1258] := 1;v[1259] := 1;v[1260] := 1;v[1261] := 1;v[1262] := 1;v[1263] := 1;v[1264] := 1;v[1265] := 1;v[1266] := 1;v[1267] := 1;v[1268] := 1;v[1269] := 1;v[1270] := 1;v[1271] := 1;v[1272] := 1;v[1273] := 1;v[1274] := 1;v[1275] := 1;v[1276] := 1;v[1277] := 1;v[1278] := 1;v[1279] := 1;v[1280] := 1;v[1281] := 1;v[1282] := 1;v[1283] := 1;v[1284] := 1;v[1285] := 1;v[1286] := 1;v[1287] := 1;v[1288] := 1;v[1289] := 1;v[1290] := 1;v[1291] := 1;v[1292] := 1;v[1293] := 1;v[1294] := 1;v[1295] := 1;v[1296] := 1;v[1297] := 1;v[1298] := 1;v[1299] := 1;v[1300] := 1;v[1301] := 1;v[1302] := 1;v[1303] := 1;v[1304] := 1;v[1305] := 1;v[1306] := 1;v[1307] := 1;v[1308] := 1;v[1309] := 1;v[1310] := 1;v[1311] := 1;v[1312] := 1;v[1313] := 1;v[1314] := 1;v[1315] := 1;v[1316] := 1;v[1317] := 1;v[1318] := 1;v[1319] := 1;v[1320] := 1;v[1321] := 1;v[1322] := 1;v[1323] := 1;v[1324] := 1;v[1325] := 1;v[1326] := 1;v[1327] := 1;v[1328] := 1;v[1329] := 1;v[1330] := 1;v[1331] := 1;v[1332] := 1;v[1333] := 1;v[1334] := 1;v[1335] := 1;v[1336] := 1;v[1337] := 1;v[1338] := 1;v[1339] := 1;v[1340] := 1;v[1341] := 1;v[1342] := 1;v[1343] := 1;v[1344] := 1;v[1345] := 1;v[1346] := 1;v[1347] := 1;v[1348] := 1;v[1349] := 1;v[1350] := 1;v[1351] := 1;v[1352] := 1;v[1353] := 1;v[1354] := 1;v[1355] := 1;v[1356] := 1;v[1357] := 1;v[1358] := 1;v[1359] := 1;v[1360] := 1;v[1361] := 1;v[1362] := 1;v[1363] := 1;v[1364] := 1;v[1365] := 1;v[1366] := 1;v[1367] := 1;v[1368] := 1;v[1369] := 1;v[1370] := 1;v[1371] := 1;v[1372] := 1;v[1373] := 1;v[1374] := 1;v[1375] := 1;v[1376] := 1;v[1377] := 1;v[1378] := 1;v[1379] := 1;v[1380] := 1;v[1381] := 1;v[1382] := 1;v[1383] := 1;v[1384] := 1;v[1385] := 1;v[1386] := 1;v[1387] := 1;v[1388] := 1;v[1389] := 1;v[1390] := 1;v[1391] := 1;v[1392] := 1;v[1393] := 1;v[1394] := 1;v[1395] := 1;v[1396] := 1;v[1397] := 1;v[1398] := 1;v[1399] := 1;v[1400] := 1;v[1401] := 1;v[1402] := 1;v[1403] := 1;v[1404] := 1;v[1405] := 1;v[1406] := 1;v[1407] := 1;v[1408] := 1;v[1409] := 1;v[1410] := 1;v[1411] := 1;v[1412] := 1;v[1413] := 1;v[1414] := 1;v[1415] := 1;v[1416] := 1;v[1417] := 1;v[1418] := 1;v[1419] := 1;v[1420] := 1;v[1421] := 1;v[1422] := 1;v[1423] := 1;v[1424] := 1;v[1425] := 1;v[1426] := 1;v[1427] := 1;v[1428] := 1;v[1429] := 1;v[1430] := 1;v[1431] := 1;v[1432] := 1;v[1433] := 1;v[1434] := 1;v[1435] := 1;v[1436] := 1;v[1437] := 1;v[1438] := 1;v[1439] := 1;v[1440] := 1;v[1441] := 1;v[1442] := 1;v[1443] := 1;v[1444] := 1;v[1445] := 1;v[1446] := 1;v[1447] := 1;v[1448] := 1;v[1449] := 1;v[1450] := 1;v[1451] := 1;v[1452] := 1;v[1453] := 1;v[1454] := 1;v[1455] := 1;v[1456] := 1;v[1457] := 1;v[1458] := 1;v[1459] := 1;v[1460] := 1;v[1461] := 1;v[1462] := 1;v[1463] := 1;v[1464] := 1;v[1465] := 1;v[1466] := 1;v[1467] := 1;v[1468] := 1;v[1469] := 1;v[1470] := 1;v[1471] := 1;v[1472] := 1;v[1473] := 1;v[1474] := 1;v[1475] := 1;v[1476] := 1;v[1477] := 1;v[1478] := 1;v[1479] := 1;v[1480] := 1;v[1481] := 1;v[1482] := 1;v[1483] := 1;v[1484] := 1;v[1485] := 1;v[1486] := 1;v[1487] := 1;v[1488] := 1;v[1489] := 1;v[1490] := 1;v[1491] := 1;v[1492] := 1;v[1493] := 1;v[1494] := 1;v[1495] := 1;v[1496] := 1;v[1497] := 1;v[1498] := 1;v[1499] := 1;v[1500] := 1;v[1501] := 1;v[1502] := 1;v[1503] := 1;v[1504] := 1;v[1505] := 1;v[1506] := 1;v[1507] := 1;v[1508] := 1;v[1509] := 1;v[1510] := 1;v[1511] := 1;v[1512] := 1;v[1513] := 1;v[1514] := 1;v[1515] := 1;v[1516] := 1;v[1517] := 1;v[1518] := 1;v[1519] := 1;v[1520] := 1;v[1521] := 1;v[1522] := 1;v[1523] := 1;v[1524] := 1;v[1525] := 1;v[1526] := 1;v[1527] := 1;v[1528] := 1;v[1529] := 1;v[1530] := 1;v[1531] := 1;v[1532] := 1;v[1533] := 1;v[1534] := 1;v[1535] := 1;v[1536] := 1;v[1537] := 1;v[1538] := 1;v[1539] := 1;v[1540] := 1;v[1541] := 1;v[1542] := 1;v[1543] := 1;v[1544] := 1;v[1545] := 1;v[1546] := 1;v[1547] := 1;v[1548] := 1;v[1549] := 1;v[1550] := 1;v[1551] := 1;v[1552] := 1;v[1553] := 1;v[1554] := 1;v[1555] := 1;v[1556] := 1;v[1557] := 1;v[1558] := 1;v[1559] := 1;v[1560] := 1;v[1561] := 1;v[1562] := 1;v[1563] := 1;v[1564] := 1;v[1565] := 1;v[1566] := 1;v[1567] := 1;v[1568] := 1;v[1569] := 1;v[1570] := 1;v[1571] := 1;v[1572] := 1;v[1573] := 1;v[1574] := 1;v[1575] := 1;v[1576] := 1;v[1577] := 1;v[1578] := 1;v[1579] := 1;v[1580] := 1;v[1581] := 1;v[1582] := 1;v[1583] := 1;v[1584] := 1;v[1585] := 1;v[1586] := 1;v[1587] := 1;v[1588] := 1;v[1589] := 1;v[1590] := 1;v[1591] := 1;v[1592] := 1;v[1593] := 1;v[1594] := 1;v[1595] := 1;v[1596] := 1;v[1597] := 1;v[1598] := 1;v[1599] := 1;v[1600] := 1;v[1601] := 1;v[1602] := 1;v[1603] := 1;v[1604] := 1;v[1605] := 1;v[1606] := 1;v[1607] := 1;v[1608] := 1;v[1609] := 1;v[1610] := 1;v[1611] := 1;v[1612] := 1;v[1613] := 1;v[1614] := 1;v[1615] := 1;v[1616] := 1;v[1617] := 1;v[1618] := 1;v[1619] := 1;v[1620] := 1;v[1621] := 1;v[1622] := 1;v[1623] := 1;v[1624] := 1;v[1625] := 1;v[1626] := 1;v[1627] := 1;v[1628] := 1;v[1629] := 1;v[1630] := 1;v[1631] := 1;v[1632] := 1;v[1633] := 1;v[1634] := 1;v[1635] := 1;v[1636] := 1;v[1637] := 1;v[1638] := 1;v[1639] := 1;v[1640] := 1;v[1641] := 1;v[1642] := 1;v[1643] := 1;v[1644] := 1;v[1645] := 1;v[1646] := 1;v[1647] := 1;v[1648] := 1;v[1649] := 1;v[1650] := 1;v[1651] := 1;v[1652] := 1;v[1653] := 1;v[1654] := 1;v[1655] := 1;v[1656] := 1;v[1657] := 1;v[1658] := 1;v[1659] := 1;v[1660] := 1;v[1661] := 1;v[1662] := 1;v[1663] := 1;v[1664] := 1;v[1665] := 1;v[1666] := 1;v[1667] := 1;v[1668] := 1;v[1669] := 1;v[1670] := 1;v[1671] := 1;v[1672] := 1;v[1673] := 1;v[1674] := 1;v[1675] := 1;v[1676] := 1;v[1677] := 1;v[1678] := 1;v[1679] := 1;v[1680] := 1;v[1681] := 1;v[1682] := 1;v[1683] := 1;v[1684] := 1;v[1685] := 1;v[1686] := 1;v[1687] := 1;v[1688] := 1;v[1689] := 1;v[1690] := 1;v[1691] := 1;v[1692] := 1;v[1693] := 1;v[1694] := 1;v[1695] := 1;v[1696] := 1;v[1697] := 1;v[1698] := 1;v[1699] := 1;v[1700] := 1;v[1701] := 1;v[1702] := 1;v[1703] := 1;v[1704] := 1;v[1705] := 1;v[1706] := 1;v[1707] := 1;v[1708] := 1;v[1709] := 1;v[1710] := 1;v[1711] := 1;v[1712] := 1;v[1713] := 1;v[1714] := 1;v[1715] := 1;v[1716] := 1;v[1717] := 1;v[1718] := 1;v[1719] := 1;v[1720] := 1;v[1721] := 1;v[1722] := 1;v[1723] := 1;v[1724] := 1;v[1725] := 1;v[1726] := 1;v[1727] := 1;v[1728] := 1;v[1729] := 1;v[1730] := 1;v[1731] := 1;v[1732] := 1;v[1733] := 1;v[1734] := 1;v[1735] := 1;v[1736] := 1;v[1737] := 1;v[1738] := 1;v[1739] := 1;v[1740] := 1;v[1741] := 1;v[1742] := 1;v[1743] := 1;v[1744] := 1;v[1745] := 1;v[1746] := 1;v[1747] := 1;v[1748] := 1;v[1749] := 1;v[1750] := 1;v[1751] := 1;v[1752] := 1;v[1753] := 1;v[1754] := 1;v[1755] := 1;v[1756] := 1;v[1757] := 1;v[1758] := 1;v[1759] := 1;v[1760] := 1;v[1761] := 1;v[1762] := 1;v[1763] := 1;v[1764] := 1;v[1765] := 1;v[1766] := 1;v[1767] := 1;v[1768] := 1;v[1769] := 1;v[1770] := 1;v[1771] := 1;v[1772] := 1;v[1773] := 1;v[1774] := 1;v[1775] := 1;v[1776] := 1;v[1777] := 1;v[1778] := 1;v[1779] := 1;v[1780] := 1;v[1781] := 1;v[1782] := 1;v[1783] := 1;v[1784] := 1;v[1785] := 1;v[1786] := 1;v[1787] := 1;v[1788] := 1;v[1789] := 1;v[1790] := 1;v[1791] := 1;v[1792] := 1;v[1793] := 1;v[1794] := 1;v[1795] := 1;v[1796] := 1;v[1797] := 1;v[1798] := 1;v[1799] := 1;v[1800] := 1;v[1801] := 1;v[1802] := 1;v[1803] := 1;v[1804] := 1;v[1805] := 1;v[1806] := 1;v[1807] := 1;v[1808] := 1;v[1809] := 1;v[1810] := 1;v[1811] := 1;v[1812] := 1;v[1813] := 1;v[1814] := 1;v[1815] := 1;v[1816] := 1;v[1817] := 1;v[1818] := 1;v[1819] := 1;v[1820] := 1;v[1821] := 1;v[1822] := 1;v[1823] := 1;v[1824] := 1;v[1825] := 1;v[1826] := 1;v[1827] := 1;v[1828] := 1;v[1829] := 1;v[1830] := 1;v[1831] := 1;v[1832] := 1;v[1833] := 1;v[1834] := 1;v[1835] := 1;v[1836] := 1;v[1837] := 1;v[1838] := 1;v[1839] := 1;v[1840] := 1;v[1841] := 1;v[1842] := 1;v[1843] := 1;v[1844] := 1;v[1845] := 1;v[1846] := 1;v[1847] := 1;v[1848] := 1;v[1849] := 1;v[1850] := 1;v[1851] := 1;v[1852] := 1;v[1853] := 1;v[1854] := 1;v[1855] := 1;v[1856] := 1;v[1857] := 1;v[1858] := 1;v[1859] := 1;v[1860] := 1;v[1861] := 1;v[1862] := 1;v[1863] := 1;v[1864] := 1;v[1865] := 1;v[1866] := 1;v[1867] := 1;v[1868] := 1;v[1869] := 1;v[1870] := 1;v[1871] := 1;v[1872] := 1;v[1873] := 1;v[1874] := 1;v[1875] := 1;v[1876] := 1;v[1877] := 1;v[1878] := 1;v[1879] := 1;v[1880] := 1;v[1881] := 1;v[1882] := 1;v[1883] := 1;v[1884] := 1;v[1885] := 1;v[1886] := 1;v[1887] := 1;v[1888] := 1;v[1889] := 1;v[1890] := 1;v[1891] := 1;v[1892] := 1;v[1893] := 1;v[1894] := 1;v[1895] := 1;v[1896] := 1;v[1897] := 1;v[1898] := 1;v[1899] := 1;v[1900] := 1;v[1901] := 1;v[1902] := 1;v[1903] := 1;v[1904] := 1;v[1905] := 1;v[1906] := 1;v[1907] := 1;v[1908] := 1;v[1909] := 1;v[1910] := 1;v[1911] := 1;v[1912] := 1;v[1913] := 1;v[1914] := 1;v[1915] := 1;v[1916] := 1;v[1917] := 1;v[1918] := 1;v[1919] := 1;v[1920] := 1;v[1921] := 1;v[1922] := 1;v[1923] := 1;v[1924] := 1;v[1925] := 1;v[1926] := 1;v[1927] := 1;v[1928] := 1;v[1929] := 1;v[1930] := 1;v[1931] := 1;v[1932] := 1;v[1933] := 1;v[1934] := 1;v[1935] := 1;v[1936] := 1;v[1937] := 1;v[1938] := 1;v[1939] := 1;v[1940] := 1;v[1941] := 1;v[1942] := 1;v[1943] := 1;v[1944] := 1;v[1945] := 1;v[1946] := 1;v[1947] := 1;v[1948] := 1;v[1949] := 1;v[1950] := 1;v[1951] := 1;v[1952] := 1;v[1953] := 1;v[1954] := 1;v[1955] := 1;v[1956] := 1;v[1957] := 1;v[1958] := 1;v[1959] := 1;v[1960] := 1;v[1961] := 1;v[1962] := 1;v[1963] := 1;v[1964] := 1;v[1965] := 1;v[1966] := 1;v[1967] := 1;v[1968] := 1;v[1969] := 1;v[1970] := 1;v[1971] := 1;v[1972] := 1;v[1973] := 1;v[1974] := 1;v[1975] := 1;v[1976] := 1;v[1977] := 1;v[1978] := 1;v[1979] := 1;v[1980] := 1;v[1981] := 1;v[1982] := 1;v[1983] := 1;v[1984] := 1;v[1985] := 1;v[1986] := 1;v[1987] := 1;v[1988] := 1;v[1989] := 1;v[1990] := 1;v[1991] := 1;v[1992] := 1;v[1993] := 1;v[1994] := 1;v[1995] := 1;v[1996] := 1;v[1997] := 1;v[1998] := 1;v[1999] := 1;v[2000] := 1;v[2001] := 1;v[2002] := 1;v[2003] := 1;v[2004] := 1;v[2005] := 1;v[2006] := 1;v[2007] := 1;v[2008] := 1;v[2009] := 1;v[2010] := 1;v[2011] := 1;v[2012] := 1;v[2013] := 1;v[2014] := 1;v[2015] := 1;v[2016] := 1;v[2017] := 1;v[2018] := 1;v[2019] := 1;v[2020] := 1;v[2021] := 1;v[2022] := 1;v[2023] := 1;v[2024] := 1;v[2025] := 1;v[2026] := 1;v[2027] := 1;v[2028] := 1;v[2029] := 1;v[2030] := 1;v[2031] := 1;v[2032] := 1;v[2033] := 1;v[2034] := 1;v[2035] := 1;v[2036] := 1;v[2037] := 1;v[2038] := 1;v[2039] := 1;v[2040] := 1;v[2041] := 1;v[2042] := 1;v[2043] := 1;v[2044] := 1;v[2045] := 1;v[2046] := 1;v[2047] := 1;v[2048] := 1;v[2049] := 1;v[2050] := 1;v[2051] := 1;v[2052] := 1;v[2053] := 1;v[2054] := 1;v[2055] := 1;v[2056] := 1;v[2057] := 1;v[2058] := 1;v[2059] := 1;v[2060] := 1;v[2061] := 1;v[2062] := 1;v[2063] := 1;v[2064] := 1;v[2065] := 1;v[2066] := 1;v[2067] := 1;v[2068] := 1;v[2069] := 1;v[2070] := 1;v[2071] := 1;v[2072] := 1;v[2073] := 1;v[2074] := 1;v[2075] := 1;v[2076] := 1;v[2077] := 1;v[2078] := 1;v[2079] := 1;v[2080] := 1;v[2081] := 1;v[2082] := 1;v[2083] := 1;v[2084] := 1;v[2085] := 1;v[2086] := 1;v[2087] := 1;v[2088] := 1;v[2089] := 1;v[2090] := 1;v[2091] := 1;v[2092] := 1;v[2093] := 1;v[2094] := 1;v[2095] := 1;v[2096] := 1;v[2097] := 1;v[2098] := 1;v[2099] := 1;v[2100] := 1;v[2101] := 1;v[2102] := 1;v[2103] := 1;v[2104] := 1;v[2105] := 1;v[2106] := 1;v[2107] := 1;v[2108] := 1;v[2109] := 1;v[2110] := 1;v[2111] := 1;v[2112] := 1;v[2113] := 1;v[2114] := 1;v[2115] := 1;v[2116] := 1;v[2117] := 1;v[2118] := 1;v[2119] := 1;v[2120] := 1;v[2121] := 1;v[2122] := 1;v[2123] := 1;v[2124] := 1;v[2125] := 1;v[2126] := 1;v[2127] := 1;v[2128] := 1;v[2129] := 1;v[2130] := 1;v[2131] := 1;v[2132] := 1;v[2133] := 1;v[2134] := 1;v[2135] := 1;v[2136] := 1;v[2137] := 1;v[2138] := 1;v[2139] := 1;v[2140] := 1;v[2141] := 1;v[2142] := 1;v[2143] := 1;v[2144] := 1;v[2145] := 1;v[2146] := 1;v[2147] := 1;v[2148] := 1;v[2149] := 1;v[2150] := 1;v[2151] := 1;v[2152] := 1;v[2153] := 1;v[2154] := 1;v[2155] := 1;v[2156] := 1;v[2157] := 1;v[2158] := 1;v[2159] := 1;v[2160] := 1;v[2161] := 1;v[2162] := 1;v[2163] := 1;v[2164] := 1;v[2165] := 1;v[2166] := 1;v[2167] := 1;v[2168] := 1;v[2169] := 1;v[2170] := 1;v[2171] := 1;v[2172] := 1;v[2173] := 1;v[2174] := 1;v[2175] := 1;v[2176] := 1;v[2177] := 1;v[2178] := 1;v[2179] := 1;v[2180] := 1;v[2181] := 1;v[2182] := 1;v[2183] := 1;v[2184] := 1;v[2185] := 1;v[2186] := 1;v[2187] := 1;v[2188] := 1;v[2189] := 1;v[2190] := 1;v[2191] := 1;v[2192] := 1;v[2193] := 1;v[2194] := 1;v[2195] := 1;v[2196] := 1;v[2197] := 1;v[2198] := 1;v[2199] := 1;v[2200] := 1;v[2201] := 1;v[2202] := 1;v[2203] := 1;v[2204] := 1;v[2205] := 1;v[2206] := 1;v[2207] := 1;v[2208] := 1;v[2209] := 1;v[2210] := 1;v[2211] := 1;v[2212] := 1;v[2213] := 1;v[2214] := 1;v[2215] := 1;v[2216] := 1;v[2217] := 1;v[2218] := 1;v[2219] := 1;v[2220] := 1;v[2221] := 1;v[2222] := 1;v[2223] := 1;v[2224] := 1;v[2225] := 1;v[2226] := 1;v[2227] := 1;v[2228] := 1;v[2229] := 1;v[2230] := 1;v[2231] := 1;v[2232] := 1;v[2233] := 1;v[2234] := 1;v[2235] := 1;v[2236] := 1;v[2237] := 1;v[2238] := 1;v[2239] := 1;v[2240] := 1;v[2241] := 1;v[2242] := 1;v[2243] := 1;v[2244] := 1;v[2245] := 1;v[2246] := 1;v[2247] := 1;v[2248] := 1;v[2249] := 1;v[2250] := 1;v[2251] := 1;v[2252] := 1;v[2253] := 1;v[2254] := 1;v[2255] := 1;v[2256] := 1;v[2257] := 1;v[2258] := 1;v[2259] := 1;v[2260] := 1;v[2261] := 1;v[2262] := 1;v[2263] := 1;v[2264] := 1;v[2265] := 1;v[2266] := 1;v[2267] := 1;v[2268] := 1;v[2269] := 1;v[2270] := 1;v[2271] := 1;v[2272] := 1;v[2273] := 1;v[2274] := 1;v[2275] := 1;v[2276] := 1;v[2277] := 1;v[2278] := 1;v[2279] := 1;v[2280] := 1;v[2281] := 1;v[2282] := 1;v[2283] := 1;v[2284] := 1;v[2285] := 1;v[2286] := 1;v[2287] := 1;v[2288] := 1;v[2289] := 1;v[2290] := 1;v[2291] := 1;v[2292] := 1;v[2293] := 1;v[2294] := 1;v[2295] := 1;v[2296] := 1;v[2297] := 1;v[2298] := 1;v[2299] := 1;v[2300] := 1;v[2301] := 1;v[2302] := 1;v[2303] := 1;v[2304] := 1;v[2305] := 1;v[2306] := 1;v[2307] := 1;v[2308] := 1;v[2309] := 1;v[2310] := 1;v[2311] := 1;v[2312] := 1;v[2313] := 1;v[2314] := 1;v[2315] := 1;v[2316] := 1;v[2317] := 1;v[2318] := 1;v[2319] := 1;v[2320] := 1;v[2321] := 1;v[2322] := 1;v[2323] := 1;v[2324] := 1;v[2325] := 1;v[2326] := 1;v[2327] := 1;v[2328] := 1;v[2329] := 1;v[2330] := 1;v[2331] := 1;v[2332] := 1;v[2333] := 1;v[2334] := 1;v[2335] := 1;v[2336] := 1;v[2337] := 1;v[2338] := 1;v[2339] := 1;v[2340] := 1;v[2341] := 1;v[2342] := 1;v[2343] := 1;v[2344] := 1;v[2345] := 1;v[2346] := 1;v[2347] := 1;v[2348] := 1;v[2349] := 1;v[2350] := 1;v[2351] := 1;v[2352] := 1;v[2353] := 1;v[2354] := 1;v[2355] := 1;v[2356] := 1;v[2357] := 1;v[2358] := 1;v[2359] := 1;v[2360] := 1;v[2361] := 1;v[2362] := 1;v[2363] := 1;v[2364] := 1;v[2365] := 1;v[2366] := 1;v[2367] := 1;v[2368] := 1;v[2369] := 1;v[2370] := 1;v[2371] := 1;v[2372] := 1;v[2373] := 1;v[2374] := 1;v[2375] := 1;v[2376] := 1;v[2377] := 1;v[2378] := 1;v[2379] := 1;v[2380] := 1;v[2381] := 1;v[2382] := 1;v[2383] := 1;v[2384] := 1;v[2385] := 1;v[2386] := 1;v[2387] := 1;v[2388] := 1;v[2389] := 1;v[2390] := 1;v[2391] := 1;v[2392] := 1;v[2393] := 1;v[2394] := 1;v[2395] := 1;v[2396] := 1;v[2397] := 1;v[2398] := 1;v[2399] := 1;v[2400] := 1;v[2401] := 1;v[2402] := 1;v[2403] := 1;v[2404] := 1;v[2405] := 1;v[2406] := 1;v[2407] := 1;v[2408] := 1;v[2409] := 1;v[2410] := 1;v[2411] := 1;v[2412] := 1;v[2413] := 1;v[2414] := 1;v[2415] := 1;v[2416] := 1;v[2417] := 1;v[2418] := 1;v[2419] := 1;v[2420] := 1;v[2421] := 1;v[2422] := 1;v[2423] := 1;v[2424] := 1;v[2425] := 1;v[2426] := 1;v[2427] := 1;v[2428] := 1;v[2429] := 1;v[2430] := 1;v[2431] := 1;v[2432] := 1;v[2433] := 1;v[2434] := 1;v[2435] := 1;v[2436] := 1;v[2437] := 1;v[2438] := 1;v[2439] := 1;v[2440] := 1;v[2441] := 1;v[2442] := 1;v[2443] := 1;v[2444] := 1;v[2445] := 1;v[2446] := 1;v[2447] := 1;v[2448] := 1;v[2449] := 1;v[2450] := 1;v[2451] := 1;v[2452] := 1;v[2453] := 1;v[2454] := 1;v[2455] := 1;v[2456] := 1;v[2457] := 1;v[2458] := 1;v[2459] := 1;v[2460] := 1;v[2461] := 1;v[2462] := 1;v[2463] := 1;v[2464] := 1;v[2465] := 1;v[2466] := 1;v[2467] := 1;v[2468] := 1;v[2469] := 1;v[2470] := 1;v[2471] := 1;v[2472] := 1;v[2473] := 1;v[2474] := 1;v[2475] := 1;v[2476] := 1;v[2477] := 1;v[2478] := 1;v[2479] := 1;v[2480] := 1;v[2481] := 1;v[2482] := 1;v[2483] := 1;v[2484] := 1;v[2485] := 1;v[2486] := 1;v[2487] := 1;v[2488] := 1;v[2489] := 1;v[2490] := 1;v[2491] := 1;v[2492] := 1;v[2493] := 1;v[2494] := 1;v[2495] := 1;v[2496] := 1;v[2497] := 1;v[2498] := 1;v[2499] := 1;v[2500] := 1;v[2501] := 1;v[2502] := 1;v[2503] := 1;v[2504] := 1;v[2505] := 1;v[2506] := 1;v[2507] := 1;v[2508] := 1;v[2509] := 1;v[2510] := 1;v[2511] := 1;v[2512] := 1;v[2513] := 1;v[2514] := 1;v[2515] := 1;v[2516] := 1;v[2517] := 1;v[2518] := 1;v[2519] := 1;v[2520] := 1;v[2521] := 1;v[2522] := 1;v[2523] := 1;v[2524] := 1;v[2525] := 1;v[2526] := 1;v[2527] := 1;v[2528] := 1;v[2529] := 1;v[2530] := 1;v[2531] := 1;v[2532] := 1;v[2533] := 1;v[2534] := 1;v[2535] := 1;v[2536] := 1;v[2537] := 1;v[2538] := 1;v[2539] := 1;v[2540] := 1;v[2541] := 1;v[2542] := 1;v[2543] := 1;v[2544] := 1;v[2545] := 1;v[2546] := 1;v[2547] := 1;v[2548] := 1;v[2549] := 1;v[2550] := 1;v[2551] := 1;v[2552] := 1;v[2553] := 1;v[2554] := 1;v[2555] := 1;v[2556] := 1;v[2557] := 1;v[2558] := 1;v[2559] := 1;v[2560] := 1;v[2561] := 1;v[2562] := 1;v[2563] := 1;v[2564] := 1;v[2565] := 1;v[2566] := 1;v[2567] := 1;v[2568] := 1;v[2569] := 1;v[2570] := 1;v[2571] := 1;v[2572] := 1;v[2573] := 1;v[2574] := 1;v[2575] := 1;v[2576] := 1;v[2577] := 1;v[2578] := 1;v[2579] := 1;v[2580] := 1;v[2581] := 1;v[2582] := 1;v[2583] := 1;v[2584] := 1;v[2585] := 1;v[2586] := 1;v[2587] := 1;v[2588] := 1;v[2589] := 1;v[2590] := 1;v[2591] := 1;v[2592] := 1;v[2593] := 1;v[2594] := 1;v[2595] := 1;v[2596] := 1;v[2597] := 1;v[2598] := 1;v[2599] := 1;v[2600] := 1;v[2601] := 1;v[2602] := 1;v[2603] := 1;v[2604] := 1;v[2605] := 1;v[2606] := 1;v[2607] := 1;v[2608] := 1;v[2609] := 1;v[2610] := 1;v[2611] := 1;v[2612] := 1;v[2613] := 1;v[2614] := 1;v[2615] := 1;v[2616] := 1;v[2617] := 1;v[2618] := 1;v[2619] := 1;v[2620] := 1;v[2621] := 1;v[2622] := 1;v[2623] := 1;v[2624] := 1;v[2625] := 1;v[2626] := 1;v[2627] := 1;v[2628] := 1;v[2629] := 1;v[2630] := 1;v[2631] := 1;v[2632] := 1;v[2633] := 1;v[2634] := 1;v[2635] := 1;v[2636] := 1;v[2637] := 1;v[2638] := 1;v[2639] := 1;v[2640] := 1;v[2641] := 1;v[2642] := 1;v[2643] := 1;v[2644] := 1;v[2645] := 1;v[2646] := 1;v[2647] := 1;v[2648] := 1;v[2649] := 1;v[2650] := 1;v[2651] := 1;v[2652] := 1;v[2653] := 1;v[2654] := 1;v[2655] := 1;v[2656] := 1;v[2657] := 1;v[2658] := 1;v[2659] := 1;v[2660] := 1;v[2661] := 1;v[2662] := 1;v[2663] := 1;v[2664] := 1;v[2665] := 1;v[2666] := 1;v[2667] := 1;v[2668] := 1;v[2669] := 1;v[2670] := 1;v[2671] := 1;v[2672] := 1;v[2673] := 1;v[2674] := 1;v[2675] := 1;v[2676] := 1;v[2677] := 1;v[2678] := 1;v[2679] := 1;v[2680] := 1;v[2681] := 1;v[2682] := 1;v[2683] := 1;v[2684] := 1;v[2685] := 1;v[2686] := 1;v[2687] := 1;v[2688] := 1;v[2689] := 1;v[2690] := 1;v[2691] := 1;v[2692] := 1;v[2693] := 1;v[2694] := 1;v[2695] := 1;v[2696] := 1;v[2697] := 1;v[2698] := 1;v[2699] := 1;v[2700] := 1;v[2701] := 1;v[2702] := 1;v[2703] := 1;v[2704] := 1;v[2705] := 1;v[2706] := 1;v[2707] := 1;v[2708] := 1;v[2709] := 1;v[2710] := 1;v[2711] := 1;v[2712] := 1;v[2713] := 1;v[2714] := 1;v[2715] := 1;v[2716] := 1;v[2717] := 1;v[2718] := 1;v[2719] := 1;v[2720] := 1;v[2721] := 1;v[2722] := 1;v[2723] := 1;v[2724] := 1;v[2725] := 1;v[2726] := 1;v[2727] := 1;v[2728] := 1;v[2729] := 1;v[2730] := 1;v[2731] := 1;v[2732] := 1;v[2733] := 1;v[2734] := 1;v[2735] := 1;v[2736] := 1;v[2737] := 1;v[2738] := 1;v[2739] := 1;v[2740] := 1;v[2741] := 1;v[2742] := 1;v[2743] := 1;v[2744] := 1;v[2745] := 1;v[2746] := 1;v[2747] := 1;v[2748] := 1;v[2749] := 1;v[2750] := 1;v[2751] := 1;v[2752] := 1;v[2753] := 1;v[2754] := 1;v[2755] := 1;v[2756] := 1;v[2757] := 1;v[2758] := 1;v[2759] := 1;v[2760] := 1;v[2761] := 1;v[2762] := 1;v[2763] := 1;v[2764] := 1;v[2765] := 1;v[2766] := 1;v[2767] := 1;v[2768] := 1;v[2769] := 1;v[2770] := 1;v[2771] := 1;v[2772] := 1;v[2773] := 1;v[2774] := 1;v[2775] := 1;v[2776] := 1;v[2777] := 1;v[2778] := 1;v[2779] := 1;v[2780] := 1;v[2781] := 1;v[2782] := 1;v[2783] := 1;v[2784] := 1;v[2785] := 1;v[2786] := 1;v[2787] := 1;v[2788] := 1;v[2789] := 1;v[2790] := 1;v[2791] := 1;v[2792] := 1;v[2793] := 1;v[2794] := 1;v[2795] := 1;v[2796] := 1;v[2797] := 1;v[2798] := 1;v[2799] := 1;v[2800] := 1;v[2801] := 1;v[2802] := 1;v[2803] := 1;v[2804] := 1;v[2805] := 1;v[2806] := 1;v[2807] := 1;v[2808] := 1;v[2809] := 1;v[2810] := 1;v[2811] := 1;v[2812] := 1;v[2813] := 1;v[2814] := 1;v[2815] := 1;v[2816] := 1;v[2817] := 1;v[2818] := 1;v[2819] := 1;v[2820] := 1;v[2821] := 1;v[2822] := 1;v[2823] := 1;v[2824] := 1;v[2825] := 1;v[2826] := 1;v[2827] := 1;v[2828] := 1;v[2829] := 1;v[2830] := 1;v[2831] := 1;v[2832] := 1;v[2833] := 1;v[2834] := 1;v[2835] := 1;v[2836] := 1;v[2837] := 1;v[2838] := 1;v[2839] := 1;v[2840] := 1;v[2841] := 1;v[2842] := 1;v[2843] := 1;v[2844] := 1;v[2845] := 1;v[2846] := 1;v[2847] := 1;v[2848] := 1;v[2849] := 1;v[2850] := 1;v[2851] := 1;v[2852] := 1;v[2853] := 1;v[2854] := 1;v[2855] := 1;v[2856] := 1;v[2857] := 1;v[2858] := 1;v[2859] := 1;v[2860] := 1;v[2861] := 1;v[2862] := 1;v[2863] := 1;v[2864] := 1;v[2865] := 1;v[2866] := 1;v[2867] := 1;v[2868] := 1;v[2869] := 1;v[2870] := 1;v[2871] := 1;v[2872] := 1;v[2873] := 1;v[2874] := 1;v[2875] := 1;v[2876] := 1;v[2877] := 1;v[2878] := 1;v[2879] := 1;v[2880] := 1;v[2881] := 1;v[2882] := 1;v[2883] := 1;v[2884] := 1;v[2885] := 1;v[2886] := 1;v[2887] := 1;v[2888] := 1;v[2889] := 1;v[2890] := 1;v[2891] := 1;v[2892] := 1;v[2893] := 1;v[2894] := 1;v[2895] := 1;v[2896] := 1;v[2897] := 1;v[2898] := 1;v[2899] := 1;v[2900] := 1;v[2901] := 1;v[2902] := 1;v[2903] := 1;v[2904] := 1;v[2905] := 1;v[2906] := 1;v[2907] := 1;v[2908] := 1;v[2909] := 1;v[2910] := 1;v[2911] := 1;v[2912] := 1;v[2913] := 1;v[2914] := 1;v[2915] := 1;v[2916] := 1;v[2917] := 1;v[2918] := 1;v[2919] := 1;v[2920] := 1;v[2921] := 1;v[2922] := 1;v[2923] := 1;v[2924] := 1;v[2925] := 1;v[2926] := 1;v[2927] := 1;v[2928] := 1;v[2929] := 1;v[2930] := 1;v[2931] := 1;v[2932] := 1;v[2933] := 1;v[2934] := 1;v[2935] := 1;v[2936] := 1;v[2937] := 1;v[2938] := 1;v[2939] := 1;v[2940] := 1;v[2941] := 1;v[2942] := 1;v[2943] := 1;v[2944] := 1;v[2945] := 1;v[2946] := 1;v[2947] := 1;v[2948] := 1;v[2949] := 1;v[2950] := 1;v[2951] := 1;v[2952] := 1;v[2953] := 1;v[2954] := 1;v[2955] := 1;v[2956] := 1;v[2957] := 1;v[2958] := 1;v[2959] := 1;v[2960] := 1;v[2961] := 1;v[2962] := 1;v[2963] := 1;v[2964] := 1;v[2965] := 1;v[2966] := 1;v[2967] := 1;v[2968] := 1;v[2969] := 1;v[2970] := 1;v[2971] := 1;v[2972] := 1;v[2973] := 1;v[2974] := 1;v[2975] := 1;v[2976] := 1;v[2977] := 1;v[2978] := 1;v[2979] := 1;v[2980] := 1;v[2981] := 1;v[2982] := 1;v[2983] := 1;v[2984] := 1;v[2985] := 1;v[2986] := 1;v[2987] := 1;v[2988] := 1;v[2989] := 1;v[2990] := 1;v[2991] := 1;v[2992] := 1;v[2993] := 1;v[2994] := 1;v[2995] := 1;v[2996] := 1;v[2997] := 1;v[2998] := 1;v[2999] := 1;v[3000] := 1;v[3001] := 1;v[3002] := 1;v[3003] := 1;v[3004] := 1;v[3005] := 1;v[3006] := 1;v[3007] := 1;v[3008] := 1;v[3009] := 1;v[3010] := 1;v[3011] := 1;v[3012] := 1;v[3013] := 1;v[3014] := 1;v[3015] := 1;v[3016] := 1;v[3017] := 1;v[3018] := 1;v[3019] := 1;v[3020] := 1;v[3021] := 1;v[3022] := 1;v[3023] := 1;v[3024] := 1;v[3025] := 1;v[3026] := 1;v[3027] := 1;v[3028] := 1;v[3029] := 1;v[3030] := 1;v[3031] := 1;v[3032] := 1;v[3033] := 1;v[3034] := 1;v[3035] := 1;v[3036] := 1;v[3037] := 1;v[3038] := 1;v[3039] := 1;v[3040] := 1;v[3041] := 1;v[3042] := 1;v[3043] := 1;v[3044] := 1;v[3045] := 1;v[3046] := 1;v[3047] := 1;v[3048] := 1;v[3049] := 1;v[3050] := 1;v[3051] := 1;v[3052] := 1;v[3053] := 1;v[3054] := 1;v[3055] := 1;v[3056] := 1;v[3057] := 1;v[3058] := 1;v[3059] := 1;v[3060] := 1;v[3061] := 1;v[3062] := 1;v[3063] := 1;v[3064] := 1;v[3065] := 1;v[3066] := 1;v[3067] := 1;v[3068] := 1;v[3069] := 1;v[3070] := 1;v[3071] := 1;v[3072] := 1;v[3073] := 1;v[3074] := 1;v[3075] := 1;v[3076] := 1;v[3077] := 1;v[3078] := 1;v[3079] := 1;v[3080] := 1;v[3081] := 1;v[3082] := 1;v[3083] := 1;v[3084] := 1;v[3085] := 1;v[3086] := 1;v[3087] := 1;v[3088] := 1;v[3089] := 1;v[3090] := 1;v[3091] := 1;v[3092] := 1;v[3093] := 1;v[3094] := 1;v[3095] := 1;v[3096] := 1;v[3097] := 1;v[3098] := 1;v[3099] := 1;v[3100] := 1;v[3101] := 1;v[3102] := 1;v[3103] := 1;v[3104] := 1;v[3105] := 1;v[3106] := 1;v[3107] := 1;v[3108] := 1;v[3109] := 1;v[3110] := 1;v[3111] := 1;v[3112] := 1;v[3113] := 1;v[3114] := 1;v[3115] := 1;v[3116] := 1;v[3117] := 1;v[3118] := 1;v[3119] := 1;v[3120] := 1;v[3121] := 1;v[3122] := 1;v[3123] := 1;v[3124] := 1;v[3125] := 1;v[3126] := 1;v[3127] := 1;v[3128] := 1;v[3129] := 1;v[3130] := 1;v[3131] := 1;v[3132] := 1;v[3133] := 1;v[3134] := 1;v[3135] := 1;v[3136] := 1;v[3137] := 1;v[3138] := 1;v[3139] := 1;v[3140] := 1;v[3141] := 1;v[3142] := 1;v[3143] := 1;v[3144] := 1;v[3145] := 1;v[3146] := 1;v[3147] := 1;v[3148] := 1;v[3149] := 1;v[3150] := 1;v[3151] := 1;v[3152] := 1;v[3153] := 1;v[3154] := 1;v[3155] := 1;v[3156] := 1;v[3157] := 1;v[3158] := 1;v[3159] := 1;v[3160] := 1;v[3161] := 1;v[3162] := 1;v[3163] := 1;v[3164] := 1;v[3165] := 1;v[3166] := 1;v[3167] := 1;v[3168] := 1;v[3169] := 1;v[3170] := 1;v[3171] := 1;v[3172] := 1;v[3173] := 1;v[3174] := 1;v[3175] := 1;v[3176] := 1;v[3177] := 1;v[3178] := 1;v[3179] := 1;v[3180] := 1;v[3181] := 1;v[3182] := 1;v[3183] := 1;v[3184] := 1;v[3185] := 1;v[3186] := 1;v[3187] := 1;v[3188] := 1;v[3189] := 1;v[3190] := 1;v[3191] := 1;v[3192] := 1;v[3193] := 1;v[3194] := 1;v[3195] := 1;v[3196] := 1;v[3197] := 1;v[3198] := 1;v[3199] := 1;v[3200] := 1;v[3201] := 1;v[3202] := 1;v[3203] := 1;v[3204] := 1;v[3205] := 1;v[3206] := 1;v[3207] := 1;v[3208] := 1;v[3209] := 1;v[3210] := 1;v[3211] := 1;v[3212] := 1;v[3213] := 1;v[3214] := 1;v[3215] := 1;v[3216] := 1;v[3217] := 1;v[3218] := 1;v[3219] := 1;v[3220] := 1;v[3221] := 1;v[3222] := 1;v[3223] := 1;v[3224] := 1;v[3225] := 1;v[3226] := 1;v[3227] := 1;v[3228] := 1;v[3229] := 1;v[3230] := 1;v[3231] := 1;v[3232] := 1;v[3233] := 1;v[3234] := 1;v[3235] := 1;v[3236] := 1;v[3237] := 1;v[3238] := 1;v[3239] := 1;v[3240] := 1;v[3241] := 1;v[3242] := 1;v[3243] := 1;v[3244] := 1;v[3245] := 1;v[3246] := 1;v[3247] := 1;v[3248] := 1;v[3249] := 1;v[3250] := 1;v[3251] := 1;v[3252] := 1;v[3253] := 1;v[3254] := 1;v[3255] := 1;v[3256] := 1;v[3257] := 1;v[3258] := 1;v[3259] := 1;v[3260] := 1;v[3261] := 1;v[3262] := 1;v[3263] := 1;v[3264] := 1;v[3265] := 1;v[3266] := 1;v[3267] := 1;v[3268] := 1;v[3269] := 1;v[3270] := 1;v[3271] := 1;v[3272] := 1;v[3273] := 1;v[3274] := 1;v[3275] := 1;v[3276] := 1;v[3277] := 1;v[3278] := 1;v[3279] := 1;v[3280] := 1;v[3281] := 1;v[3282] := 1;v[3283] := 1;v[3284] := 1;v[3285] := 1;v[3286] := 1;v[3287] := 1;v[3288] := 1;v[3289] := 1;v[3290] := 1;v[3291] := 1;v[3292] := 1;v[3293] := 1;v[3294] := 1;v[3295] := 1;v[3296] := 1;v[3297] := 1;v[3298] := 1;v[3299] := 1;v[3300] := 1;v[3301] := 1;v[3302] := 1;v[3303] := 1;v[3304] := 1;v[3305] := 1;v[3306] := 1;v[3307] := 1;v[3308] := 1;v[3309] := 1;v[3310] := 1;v[3311] := 1;v[3312] := 1;v[3313] := 1;v[3314] := 1;v[3315] := 1;v[3316] := 1;v[3317] := 1;v[3318] := 1;v[3319] := 1;v[3320] := 1;v[3321] := 1;v[3322] := 1;v[3323] := 1;v[3324] := 1;v[3325] := 1;v[3326] := 1;v[3327] := 1;v[3328] := 1;v[3329] := 1;v[3330] := 1;v[3331] := 1;v[3332] := 1;v[3333] := 1;v[3334] := 1;v[3335] := 1;v[3336] := 1;v[3337] := 1;v[3338] := 1;v[3339] := 1;v[3340] := 1;v[3341] := 1;v[3342] := 1;v[3343] := 1;v[3344] := 1;v[3345] := 1;v[3346] := 1;v[3347] := 1;v[3348] := 1;v[3349] := 1;v[3350] := 1;v[3351] := 1;v[3352] := 1;v[3353] := 1;v[3354] := 1;v[3355] := 1;v[3356] := 1;v[3357] := 1;v[3358] := 1;v[3359] := 1;v[3360] := 1;v[3361] := 1;v[3362] := 1;v[3363] := 1;v[3364] := 1;v[3365] := 1;v[3366] := 1;v[3367] := 1;v[3368] := 1;v[3369] := 1;v[3370] := 1;v[3371] := 1;v[3372] := 1;v[3373] := 1;v[3374] := 1;v[3375] := 1;v[3376] := 1;v[3377] := 1;v[3378] := 1;v[3379] := 1;v[3380] := 1;v[3381] := 1;v[3382] := 1;v[3383] := 1;v[3384] := 1;v[3385] := 1;v[3386] := 1;v[3387] := 1;v[3388] := 1;v[3389] := 1;v[3390] := 1;v[3391] := 1;v[3392] := 1;v[3393] := 1;v[3394] := 1;v[3395] := 1;v[3396] := 1;v[3397] := 1;v[3398] := 1;v[3399] := 1;v[3400] := 1;v[3401] := 1;v[3402] := 1;v[3403] := 1;v[3404] := 1;v[3405] := 1;v[3406] := 1;v[3407] := 1;v[3408] := 1;v[3409] := 1;v[3410] := 1;v[3411] := 1;v[3412] := 1;v[3413] := 1;v[3414] := 1;v[3415] := 1;v[3416] := 1;v[3417] := 1;v[3418] := 1;v[3419] := 1;v[3420] := 1;v[3421] := 1;v[3422] := 1;v[3423] := 1;v[3424] := 1;v[3425] := 1;v[3426] := 1;v[3427] := 1;v[3428] := 1;v[3429] := 1;v[3430] := 1;v[3431] := 1;v[3432] := 1;v[3433] := 1;v[3434] := 1;v[3435] := 1;v[3436] := 1;v[3437] := 1;v[3438] := 1;v[3439] := 1;v[3440] := 1;v[3441] := 1;v[3442] := 1;v[3443] := 1;v[3444] := 1;v[3445] := 1;v[3446] := 1;v[3447] := 1;v[3448] := 1;v[3449] := 1;v[3450] := 1;v[3451] := 1;v[3452] := 1;v[3453] := 1;v[3454] := 1;v[3455] := 1;v[3456] := 1;v[3457] := 1;v[3458] := 1;v[3459] := 1;v[3460] := 1;v[3461] := 1;v[3462] := 1;v[3463] := 1;v[3464] := 1;v[3465] := 1;v[3466] := 1;v[3467] := 1;v[3468] := 1;v[3469] := 1;v[3470] := 1;v[3471] := 1;v[3472] := 1;v[3473] := 1;v[3474] := 1;v[3475] := 1;v[3476] := 1;v[3477] := 1;v[3478] := 1;v[3479] := 1;v[3480] := 1;v[3481] := 1;v[3482] := 1;v[3483] := 1;v[3484] := 1;v[3485] := 1;v[3486] := 1;v[3487] := 1;v[3488] := 1;v[3489] := 1;v[3490] := 1;v[3491] := 1;v[3492] := 1;v[3493] := 1;v[3494] := 1;v[3495] := 1;v[3496] := 1;v[3497] := 1;v[3498] := 1;v[3499] := 1;v[3500] := 1;v[3501] := 1;v[3502] := 1;v[3503] := 1;v[3504] := 1;v[3505] := 1;v[3506] := 1;v[3507] := 1;v[3508] := 1;v[3509] := 1;v[3510] := 1;v[3511] := 1;v[3512] := 1;v[3513] := 1;v[3514] := 1;v[3515] := 1;v[3516] := 1;v[3517] := 1;v[3518] := 1;v[3519] := 1;v[3520] := 1;v[3521] := 1;v[3522] := 1;v[3523] := 1;v[3524] := 1;v[3525] := 1;v[3526] := 1;v[3527] := 1;v[3528] := 1;v[3529] := 1;v[3530] := 1;v[3531] := 1;v[3532] := 1;v[3533] := 1;v[3534] := 1;v[3535] := 1;v[3536] := 1;v[3537] := 1;v[3538] := 1;v[3539] := 1;v[3540] := 1;v[3541] := 1;v[3542] := 1;v[3543] := 1;v[3544] := 1;v[3545] := 1;v[3546] := 1;v[3547] := 1;v[3548] := 1;v[3549] := 1;v[3550] := 1;v[3551] := 1;v[3552] := 1;v[3553] := 1;v[3554] := 1;v[3555] := 1;v[3556] := 1;v[3557] := 1;v[3558] := 1;v[3559] := 1;v[3560] := 1;v[3561] := 1;v[3562] := 1;v[3563] := 1;v[3564] := 1;v[3565] := 1;v[3566] := 1;v[3567] := 1;v[3568] := 1;v[3569] := 1;v[3570] := 1;v[3571] := 1;v[3572] := 1;v[3573] := 1;v[3574] := 1;v[3575] := 1;v[3576] := 1;v[3577] := 1;v[3578] := 1;v[3579] := 1;v[3580] := 1;v[3581] := 1;v[3582] := 1;v[3583] := 1;v[3584] := 1;v[3585] := 1;v[3586] := 1;v[3587] := 1;v[3588] := 1;v[3589] := 1;v[3590] := 1;v[3591] := 1;v[3592] := 1;v[3593] := 1;v[3594] := 1;v[3595] := 1;v[3596] := 1;v[3597] := 1;v[3598] := 1;v[3599] := 1;v[3600] := 1;v[3601] := 1;v[3602] := 1;v[3603] := 1;v[3604] := 1;v[3605] := 1;v[3606] := 1;v[3607] := 1;v[3608] := 1;v[3609] := 1;v[3610] := 1;v[3611] := 1;v[3612] := 1;v[3613] := 1;v[3614] := 1;v[3615] := 1;v[3616] := 1;v[3617] := 1;v[3618] := 1;v[3619] := 1;v[3620] := 1;v[3621] := 1;v[3622] := 1;v[3623] := 1;v[3624] := 1;v[3625] := 1;v[3626] := 1;v[3627] := 1;v[3628] := 1;v[3629] := 1;v[3630] := 1;v[3631] := 1;v[3632] := 1;v[3633] := 1;v[3634] := 1;v[3635] := 1;v[3636] := 1;v[3637] := 1;v[3638] := 1;v[3639] := 1;v[3640] := 1;v[3641] := 1;v[3642] := 1;v[3643] := 1;v[3644] := 1;v[3645] := 1;v[3646] := 1;v[3647] := 1;v[3648] := 1;v[3649] := 1;v[3650] := 1;v[3651] := 1;v[3652] := 1;v[3653] := 1;v[3654] := 1;v[3655] := 1;v[3656] := 1;v[3657] := 1;v[3658] := 1;v[3659] := 1;v[3660] := 1;v[3661] := 1;v[3662] := 1;v[3663] := 1;v[3664] := 1;v[3665] := 1;v[3666] := 1;v[3667] := 1;v[3668] := 1;v[3669] := 1;v[3670] := 1;v[3671] := 1;v[3672] := 1;v[3673] := 1;v[3674] := 1;v[3675] := 1;v[3676] := 1;v[3677] := 1;v[3678] := 1;v[3679] := 1;v[3680] := 1;v[3681] := 1;v[3682] := 1;v[3683] := 1;v[3684] := 1;v[3685] := 1;v[3686] := 1;v[3687] := 1;v[3688] := 1;v[3689] := 1;v[3690] := 1;v[3691] := 1;v[3692] := 1;v[3693] := 1;v[3694] := 1;v[3695] := 1;v[3696] := 1;v[3697] := 1;v[3698] := 1;v[3699] := 1;v[3700] := 1;v[3701] := 1;v[3702] := 1;v[3703] := 1;v[3704] := 1;v[3705] := 1;v[3706] := 1;v[3707] := 1;v[3708] := 1;v[3709] := 1;v[3710] := 1;v[3711] := 1;v[3712] := 1;v[3713] := 1;v[3714] := 1;v[3715] := 1;v[3716] := 1;v[3717] := 1;v[3718] := 1;v[3719] := 1;v[3720] := 1;v[3721] := 1;v[3722] := 1;v[3723] := 1;v[3724] := 1;v[3725] := 1;v[3726] := 1;v[3727] := 1;v[3728] := 1;v[3729] := 1;v[3730] := 1;v[3731] := 1;v[3732] := 1;v[3733] := 1;v[3734] := 1;v[3735] := 1;v[3736] := 1;v[3737] := 1;v[3738] := 1;v[3739] := 1;v[3740] := 1;v[3741] := 1;v[3742] := 1;v[3743] := 1;v[3744] := 1;v[3745] := 1;v[3746] := 1;v[3747] := 1;v[3748] := 1;v[3749] := 1;v[3750] := 1;v[3751] := 1;v[3752] := 1;v[3753] := 1;v[3754] := 1;v[3755] := 1;v[3756] := 1;v[3757] := 1;v[3758] := 1;v[3759] := 1;v[3760] := 1;v[3761] := 1;v[3762] := 1;v[3763] := 1;v[3764] := 1;v[3765] := 1;v[3766] := 1;v[3767] := 1;v[3768] := 1;v[3769] := 1;v[3770] := 1;v[3771] := 1;v[3772] := 1;v[3773] := 1;v[3774] := 1;v[3775] := 1;v[3776] := 1;v[3777] := 1;v[3778] := 1;v[3779] := 1;v[3780] := 1;v[3781] := 1;v[3782] := 1;v[3783] := 1;v[3784] := 1;v[3785] := 1;v[3786] := 1;v[3787] := 1;v[3788] := 1;v[3789] := 1;v[3790] := 1;v[3791] := 1;v[3792] := 1;v[3793] := 1;v[3794] := 1;v[3795] := 1;v[3796] := 1;v[3797] := 1;v[3798] := 1;v[3799] := 1;v[3800] := 1;v[3801] := 1;v[3802] := 1;v[3803] := 1;v[3804] := 1;v[3805] := 1;v[3806] := 1;v[3807] := 1;v[3808] := 1;v[3809] := 1;v[3810] := 1;v[3811] := 1;v[3812] := 1;v[3813] := 1;v[3814] := 1;v[3815] := 1;v[3816] := 1;v[3817] := 1;v[3818] := 1;v[3819] := 1;v[3820] := 1;v[3821] := 1;v[3822] := 1;v[3823] := 1;v[3824] := 1;v[3825] := 1;v[3826] := 1;v[3827] := 1;v[3828] := 1;v[3829] := 1;v[3830] := 1;v[3831] := 1;v[3832] := 1;v[3833] := 1;v[3834] := 1;v[3835] := 1;v[3836] := 1;v[3837] := 1;v[3838] := 1;v[3839] := 1;v[3840] := 1;v[3841] := 1;v[3842] := 1;v[3843] := 1;v[3844] := 1;v[3845] := 1;v[3846] := 1;v[3847] := 1;v[3848] := 1;v[3849] := 1;v[3850] := 1;v[3851] := 1;v[3852] := 1;v[3853] := 1;v[3854] := 1;v[3855] := 1;v[3856] := 1;v[3857] := 1;v[3858] := 1;v[3859] := 1;v[3860] := 1;v[3861] := 1;v[3862] := 1;v[3863] := 1;v[3864] := 1;v[3865] := 1;v[3866] := 1;v[3867] := 1;v[3868] := 1;v[3869] := 1;v[3870] := 1;v[3871] := 1;v[3872] := 1;v[3873] := 1;v[3874] := 1;v[3875] := 1;v[3876] := 1;v[3877] := 1;v[3878] := 1;v[3879] := 1;v[3880] := 1;v[3881] := 1;v[3882] := 1;v[3883] := 1;v[3884] := 1;v[3885] := 1;v[3886] := 1;v[3887] := 1;v[3888] := 1;v[3889] := 1;v[3890] := 1;v[3891] := 1;v[3892] := 1;v[3893] := 1;v[3894] := 1;v[3895] := 1;v[3896] := 1;v[3897] := 1;v[3898] := 1;v[3899] := 1;v[3900] := 1;v[3901] := 1;v[3902] := 1;v[3903] := 1;v[3904] := 1;v[3905] := 1;v[3906] := 1;v[3907] := 1;v[3908] := 1;v[3909] := 1;v[3910] := 1;v[3911] := 1;v[3912] := 1;v[3913] := 1;v[3914] := 1;v[3915] := 1;v[3916] := 1;v[3917] := 1;v[3918] := 1;v[3919] := 1;v[3920] := 1;v[3921] := 1;v[3922] := 1;v[3923] := 1;v[3924] := 1;v[3925] := 1;v[3926] := 1;v[3927] := 1;v[3928] := 1;v[3929] := 1;v[3930] := 1;v[3931] := 1;v[3932] := 1;v[3933] := 1;v[3934] := 1;v[3935] := 1;v[3936] := 1;v[3937] := 1;v[3938] := 1;v[3939] := 1;v[3940] := 1;v[3941] := 1;v[3942] := 1;v[3943] := 1;v[3944] := 1;v[3945] := 1;v[3946] := 1;v[3947] := 1;v[3948] := 1;v[3949] := 1;v[3950] := 1;v[3951] := 1;v[3952] := 1;v[3953] := 1;v[3954] := 1;v[3955] := 1;v[3956] := 1;v[3957] := 1;v[3958] := 1;v[3959] := 1;v[3960] := 1;v[3961] := 1;v[3962] := 1;v[3963] := 1;v[3964] := 1;v[3965] := 1;v[3966] := 1;v[3967] := 1;v[3968] := 1;v[3969] := 1;v[3970] := 1;v[3971] := 1;v[3972] := 1;v[3973] := 1;v[3974] := 1;v[3975] := 1;v[3976] := 1;v[3977] := 1;v[3978] := 1;v[3979] := 1;v[3980] := 1;v[3981] := 1;v[3982] := 1;v[3983] := 1;v[3984] := 1;v[3985] := 1;v[3986] := 1;v[3987] := 1;v[3988] := 1;v[3989] := 1;v[3990] := 1;v[3991] := 1;v[3992] := 1;v[3993] := 1;v[3994] := 1;v[3995] := 1;v[3996] := 1;v[3997] := 1;v[3998] := 1;v[3999] := 1;v[4000] := 1;v[4001] := 1;v[4002] := 1;v[4003] := 1;v[4004] := 1;v[4005] := 1;v[4006] := 1;v[4007] := 1;v[4008] := 1;v[4009] := 1;v[4010] := 1;v[4011] := 1;v[4012] := 1;v[4013] := 1;v[4014] := 1;v[4015] := 1;v[4016] := 1;v[4017] := 1;v[4018] := 1;v[4019] := 1;v[4020] := 1;v[4021] := 1;v[4022] := 1;v[4023] := 1;v[4024] := 1;v[4025] := 1;v[4026] := 1;v[4027] := 1;v[4028] := 1;v[4029] := 1;v[4030] := 1;v[4031] := 1;v[4032] := 1;v[4033] := 1;v[4034] := 1;v[4035] := 1;v[4036] := 1;v[4037] := 1;v[4038] := 1;v[4039] := 1;v[4040] := 1;v[4041] := 1;v[4042] := 1;v[4043] := 1;v[4044] := 1;v[4045] := 1;v[4046] := 1;v[4047] := 1;v[4048] := 1;v[4049] := 1;v[4050] := 1;v[4051] := 1;v[4052] := 1;v[4053] := 1;v[4054] := 1;v[4055] := 1;v[4056] := 1;v[4057] := 1;v[4058] := 1;v[4059] := 1;v[4060] := 1;v[4061] := 1;v[4062] := 1;v[4063] := 1;v[4064] := 1;v[4065] := 1;v[4066] := 1;v[4067] := 1;v[4068] := 1;v[4069] := 1;v[4070] := 1;v[4071] := 1;v[4072] := 1;v[4073] := 1;v[4074] := 1;v[4075] := 1;v[4076] := 1;v[4077] := 1;v[4078] := 1;v[4079] := 1;v[4080] := 1;v[4081] := 1;v[4082] := 1;v[4083] := 1;v[4084] := 1;v[4085] := 1;v[4086] := 1;v[4087] := 1;v[4088] := 1;v[4089] := 1;v[4090] := 1;v[4091] := 1;v[4092] := 1;v[4093] := 1;v[4094] := 1;v[4095] := 1;
    *[
    cmd_chan?cmd;
    [ cmd & 1 = 0 ->
    [
    ([]: j:4096:  (cmd >> 1) = j   ->  tmp := v[j]  )
    ];
    read_chan!tmp
    [] cmd & 1 = 1 ->
    skip

    ]
    ]
    }
    }

    defproc proc_2(chan?(int<13>>) cmd_chan; chan?(int<8>>) read_chan; chan?(int<%{mem.cell_bits#Int}>) write_chan) {

    int<8> v[4096];
    int<13> cmd;
    int<13> tmp;
    chp {
    v[0] := 0;v[1] := 0;v[2] := 0;v[3] := 0;v[4] := 0;v[5] := 0;v[6] := 0;v[7] := 0;v[8] := 0;v[9] := 0;v[10] := 0;v[11] := 0;v[12] := 0;v[13] := 0;v[14] := 0;v[15] := 0;v[16] := 0;v[17] := 0;v[18] := 0;v[19] := 0;v[20] := 0;v[21] := 0;v[22] := 0;v[23] := 0;v[24] := 0;v[25] := 0;v[26] := 0;v[27] := 0;v[28] := 0;v[29] := 0;v[30] := 0;v[31] := 0;v[32] := 0;v[33] := 0;v[34] := 0;v[35] := 0;v[36] := 0;v[37] := 0;v[38] := 0;v[39] := 0;v[40] := 0;v[41] := 0;v[42] := 0;v[43] := 0;v[44] := 0;v[45] := 0;v[46] := 0;v[47] := 0;v[48] := 0;v[49] := 0;v[50] := 0;v[51] := 0;v[52] := 0;v[53] := 0;v[54] := 0;v[55] := 0;v[56] := 0;v[57] := 0;v[58] := 0;v[59] := 0;v[60] := 0;v[61] := 0;v[62] := 0;v[63] := 0;v[64] := 0;v[65] := 0;v[66] := 0;v[67] := 0;v[68] := 0;v[69] := 0;v[70] := 0;v[71] := 0;v[72] := 0;v[73] := 0;v[74] := 0;v[75] := 0;v[76] := 0;v[77] := 0;v[78] := 0;v[79] := 0;v[80] := 0;v[81] := 0;v[82] := 0;v[83] := 0;v[84] := 0;v[85] := 0;v[86] := 0;v[87] := 0;v[88] := 0;v[89] := 0;v[90] := 0;v[91] := 0;v[92] := 0;v[93] := 0;v[94] := 0;v[95] := 0;v[96] := 0;v[97] := 0;v[98] := 0;v[99] := 0;v[100] := 0;v[101] := 0;v[102] := 0;v[103] := 0;v[104] := 0;v[105] := 0;v[106] := 0;v[107] := 0;v[108] := 0;v[109] := 0;v[110] := 0;v[111] := 0;v[112] := 0;v[113] := 0;v[114] := 0;v[115] := 0;v[116] := 0;v[117] := 0;v[118] := 0;v[119] := 0;v[120] := 0;v[121] := 0;v[122] := 0;v[123] := 0;v[124] := 0;v[125] := 0;v[126] := 0;v[127] := 0;v[128] := 0;v[129] := 0;v[130] := 0;v[131] := 0;v[132] := 0;v[133] := 0;v[134] := 0;v[135] := 0;v[136] := 0;v[137] := 0;v[138] := 0;v[139] := 0;v[140] := 0;v[141] := 0;v[142] := 0;v[143] := 0;v[144] := 0;v[145] := 0;v[146] := 0;v[147] := 0;v[148] := 0;v[149] := 0;v[150] := 0;v[151] := 0;v[152] := 0;v[153] := 0;v[154] := 0;v[155] := 0;v[156] := 0;v[157] := 0;v[158] := 0;v[159] := 0;v[160] := 0;v[161] := 0;v[162] := 0;v[163] := 0;v[164] := 0;v[165] := 0;v[166] := 0;v[167] := 0;v[168] := 0;v[169] := 0;v[170] := 0;v[171] := 0;v[172] := 0;v[173] := 0;v[174] := 0;v[175] := 0;v[176] := 0;v[177] := 0;v[178] := 0;v[179] := 0;v[180] := 0;v[181] := 0;v[182] := 0;v[183] := 0;v[184] := 0;v[185] := 0;v[186] := 0;v[187] := 0;v[188] := 0;v[189] := 0;v[190] := 0;v[191] := 0;v[192] := 0;v[193] := 0;v[194] := 0;v[195] := 0;v[196] := 0;v[197] := 0;v[198] := 0;v[199] := 0;v[200] := 0;v[201] := 0;v[202] := 0;v[203] := 0;v[204] := 0;v[205] := 0;v[206] := 0;v[207] := 0;v[208] := 0;v[209] := 0;v[210] := 0;v[211] := 0;v[212] := 0;v[213] := 0;v[214] := 0;v[215] := 0;v[216] := 0;v[217] := 0;v[218] := 0;v[219] := 0;v[220] := 0;v[221] := 0;v[222] := 0;v[223] := 0;v[224] := 0;v[225] := 0;v[226] := 0;v[227] := 0;v[228] := 0;v[229] := 0;v[230] := 0;v[231] := 0;v[232] := 0;v[233] := 0;v[234] := 0;v[235] := 0;v[236] := 0;v[237] := 0;v[238] := 0;v[239] := 0;v[240] := 0;v[241] := 0;v[242] := 0;v[243] := 0;v[244] := 0;v[245] := 0;v[246] := 0;v[247] := 0;v[248] := 0;v[249] := 0;v[250] := 0;v[251] := 0;v[252] := 0;v[253] := 0;v[254] := 0;v[255] := 0;v[256] := 0;v[257] := 0;v[258] := 0;v[259] := 0;v[260] := 0;v[261] := 0;v[262] := 0;v[263] := 0;v[264] := 0;v[265] := 0;v[266] := 0;v[267] := 0;v[268] := 0;v[269] := 0;v[270] := 0;v[271] := 0;v[272] := 0;v[273] := 0;v[274] := 0;v[275] := 0;v[276] := 0;v[277] := 0;v[278] := 0;v[279] := 0;v[280] := 0;v[281] := 0;v[282] := 0;v[283] := 0;v[284] := 0;v[285] := 0;v[286] := 0;v[287] := 0;v[288] := 0;v[289] := 0;v[290] := 0;v[291] := 0;v[292] := 0;v[293] := 0;v[294] := 0;v[295] := 0;v[296] := 0;v[297] := 0;v[298] := 0;v[299] := 0;v[300] := 0;v[301] := 0;v[302] := 0;v[303] := 0;v[304] := 0;v[305] := 0;v[306] := 0;v[307] := 0;v[308] := 0;v[309] := 0;v[310] := 0;v[311] := 0;v[312] := 0;v[313] := 0;v[314] := 0;v[315] := 0;v[316] := 0;v[317] := 0;v[318] := 0;v[319] := 0;v[320] := 0;v[321] := 0;v[322] := 0;v[323] := 0;v[324] := 0;v[325] := 0;v[326] := 0;v[327] := 0;v[328] := 0;v[329] := 0;v[330] := 0;v[331] := 0;v[332] := 0;v[333] := 0;v[334] := 0;v[335] := 0;v[336] := 0;v[337] := 0;v[338] := 0;v[339] := 0;v[340] := 0;v[341] := 0;v[342] := 0;v[343] := 0;v[344] := 0;v[345] := 0;v[346] := 0;v[347] := 0;v[348] := 0;v[349] := 0;v[350] := 0;v[351] := 0;v[352] := 0;v[353] := 0;v[354] := 0;v[355] := 0;v[356] := 0;v[357] := 0;v[358] := 0;v[359] := 0;v[360] := 0;v[361] := 0;v[362] := 0;v[363] := 0;v[364] := 0;v[365] := 0;v[366] := 0;v[367] := 0;v[368] := 0;v[369] := 0;v[370] := 0;v[371] := 0;v[372] := 0;v[373] := 0;v[374] := 0;v[375] := 0;v[376] := 0;v[377] := 0;v[378] := 0;v[379] := 0;v[380] := 0;v[381] := 0;v[382] := 0;v[383] := 0;v[384] := 0;v[385] := 0;v[386] := 0;v[387] := 0;v[388] := 0;v[389] := 0;v[390] := 0;v[391] := 0;v[392] := 0;v[393] := 0;v[394] := 0;v[395] := 0;v[396] := 0;v[397] := 0;v[398] := 0;v[399] := 0;v[400] := 0;v[401] := 0;v[402] := 0;v[403] := 0;v[404] := 0;v[405] := 0;v[406] := 0;v[407] := 0;v[408] := 0;v[409] := 0;v[410] := 0;v[411] := 0;v[412] := 0;v[413] := 0;v[414] := 0;v[415] := 0;v[416] := 0;v[417] := 0;v[418] := 0;v[419] := 0;v[420] := 0;v[421] := 0;v[422] := 0;v[423] := 0;v[424] := 0;v[425] := 0;v[426] := 0;v[427] := 0;v[428] := 0;v[429] := 0;v[430] := 0;v[431] := 0;v[432] := 0;v[433] := 0;v[434] := 0;v[435] := 0;v[436] := 0;v[437] := 0;v[438] := 0;v[439] := 0;v[440] := 0;v[441] := 0;v[442] := 0;v[443] := 0;v[444] := 0;v[445] := 0;v[446] := 0;v[447] := 0;v[448] := 0;v[449] := 0;v[450] := 0;v[451] := 0;v[452] := 0;v[453] := 0;v[454] := 0;v[455] := 0;v[456] := 0;v[457] := 0;v[458] := 0;v[459] := 0;v[460] := 0;v[461] := 0;v[462] := 0;v[463] := 0;v[464] := 0;v[465] := 0;v[466] := 0;v[467] := 0;v[468] := 0;v[469] := 0;v[470] := 0;v[471] := 0;v[472] := 0;v[473] := 0;v[474] := 0;v[475] := 0;v[476] := 0;v[477] := 0;v[478] := 0;v[479] := 0;v[480] := 0;v[481] := 0;v[482] := 0;v[483] := 0;v[484] := 0;v[485] := 0;v[486] := 0;v[487] := 0;v[488] := 0;v[489] := 0;v[490] := 0;v[491] := 0;v[492] := 0;v[493] := 0;v[494] := 0;v[495] := 0;v[496] := 0;v[497] := 0;v[498] := 0;v[499] := 0;v[500] := 0;v[501] := 0;v[502] := 0;v[503] := 0;v[504] := 0;v[505] := 0;v[506] := 0;v[507] := 0;v[508] := 0;v[509] := 0;v[510] := 0;v[511] := 0;v[512] := 0;v[513] := 0;v[514] := 0;v[515] := 0;v[516] := 0;v[517] := 0;v[518] := 0;v[519] := 0;v[520] := 0;v[521] := 0;v[522] := 0;v[523] := 0;v[524] := 0;v[525] := 0;v[526] := 0;v[527] := 0;v[528] := 0;v[529] := 0;v[530] := 0;v[531] := 0;v[532] := 0;v[533] := 0;v[534] := 0;v[535] := 0;v[536] := 0;v[537] := 0;v[538] := 0;v[539] := 0;v[540] := 0;v[541] := 0;v[542] := 0;v[543] := 0;v[544] := 0;v[545] := 0;v[546] := 0;v[547] := 0;v[548] := 0;v[549] := 0;v[550] := 0;v[551] := 0;v[552] := 0;v[553] := 0;v[554] := 0;v[555] := 0;v[556] := 0;v[557] := 0;v[558] := 0;v[559] := 0;v[560] := 0;v[561] := 0;v[562] := 0;v[563] := 0;v[564] := 0;v[565] := 0;v[566] := 0;v[567] := 0;v[568] := 0;v[569] := 0;v[570] := 0;v[571] := 0;v[572] := 0;v[573] := 0;v[574] := 0;v[575] := 0;v[576] := 0;v[577] := 0;v[578] := 0;v[579] := 0;v[580] := 0;v[581] := 0;v[582] := 0;v[583] := 0;v[584] := 0;v[585] := 0;v[586] := 0;v[587] := 0;v[588] := 0;v[589] := 0;v[590] := 0;v[591] := 0;v[592] := 0;v[593] := 0;v[594] := 0;v[595] := 0;v[596] := 0;v[597] := 0;v[598] := 0;v[599] := 0;v[600] := 0;v[601] := 0;v[602] := 0;v[603] := 0;v[604] := 0;v[605] := 0;v[606] := 0;v[607] := 0;v[608] := 0;v[609] := 0;v[610] := 0;v[611] := 0;v[612] := 0;v[613] := 0;v[614] := 0;v[615] := 0;v[616] := 0;v[617] := 0;v[618] := 0;v[619] := 0;v[620] := 0;v[621] := 0;v[622] := 0;v[623] := 0;v[624] := 0;v[625] := 0;v[626] := 0;v[627] := 0;v[628] := 0;v[629] := 0;v[630] := 0;v[631] := 0;v[632] := 0;v[633] := 0;v[634] := 0;v[635] := 0;v[636] := 0;v[637] := 0;v[638] := 0;v[639] := 0;v[640] := 0;v[641] := 0;v[642] := 0;v[643] := 0;v[644] := 0;v[645] := 0;v[646] := 0;v[647] := 0;v[648] := 0;v[649] := 0;v[650] := 0;v[651] := 0;v[652] := 0;v[653] := 0;v[654] := 0;v[655] := 0;v[656] := 0;v[657] := 0;v[658] := 0;v[659] := 0;v[660] := 0;v[661] := 0;v[662] := 0;v[663] := 0;v[664] := 0;v[665] := 0;v[666] := 0;v[667] := 0;v[668] := 0;v[669] := 0;v[670] := 0;v[671] := 0;v[672] := 0;v[673] := 0;v[674] := 0;v[675] := 0;v[676] := 0;v[677] := 0;v[678] := 0;v[679] := 0;v[680] := 0;v[681] := 0;v[682] := 0;v[683] := 0;v[684] := 0;v[685] := 0;v[686] := 0;v[687] := 0;v[688] := 0;v[689] := 0;v[690] := 0;v[691] := 0;v[692] := 0;v[693] := 0;v[694] := 0;v[695] := 0;v[696] := 0;v[697] := 0;v[698] := 0;v[699] := 0;v[700] := 0;v[701] := 0;v[702] := 0;v[703] := 0;v[704] := 0;v[705] := 0;v[706] := 0;v[707] := 0;v[708] := 0;v[709] := 0;v[710] := 0;v[711] := 0;v[712] := 0;v[713] := 0;v[714] := 0;v[715] := 0;v[716] := 0;v[717] := 0;v[718] := 0;v[719] := 0;v[720] := 0;v[721] := 0;v[722] := 0;v[723] := 0;v[724] := 0;v[725] := 0;v[726] := 0;v[727] := 0;v[728] := 0;v[729] := 0;v[730] := 0;v[731] := 0;v[732] := 0;v[733] := 0;v[734] := 0;v[735] := 0;v[736] := 0;v[737] := 0;v[738] := 0;v[739] := 0;v[740] := 0;v[741] := 0;v[742] := 0;v[743] := 0;v[744] := 0;v[745] := 0;v[746] := 0;v[747] := 0;v[748] := 0;v[749] := 0;v[750] := 0;v[751] := 0;v[752] := 0;v[753] := 0;v[754] := 0;v[755] := 0;v[756] := 0;v[757] := 0;v[758] := 0;v[759] := 0;v[760] := 0;v[761] := 0;v[762] := 0;v[763] := 0;v[764] := 0;v[765] := 0;v[766] := 0;v[767] := 0;v[768] := 0;v[769] := 0;v[770] := 0;v[771] := 0;v[772] := 0;v[773] := 0;v[774] := 0;v[775] := 0;v[776] := 0;v[777] := 0;v[778] := 0;v[779] := 0;v[780] := 0;v[781] := 0;v[782] := 0;v[783] := 0;v[784] := 0;v[785] := 0;v[786] := 0;v[787] := 0;v[788] := 0;v[789] := 0;v[790] := 0;v[791] := 0;v[792] := 0;v[793] := 0;v[794] := 0;v[795] := 0;v[796] := 0;v[797] := 0;v[798] := 0;v[799] := 0;v[800] := 0;v[801] := 0;v[802] := 0;v[803] := 0;v[804] := 0;v[805] := 0;v[806] := 0;v[807] := 0;v[808] := 0;v[809] := 0;v[810] := 0;v[811] := 0;v[812] := 0;v[813] := 0;v[814] := 0;v[815] := 0;v[816] := 0;v[817] := 0;v[818] := 0;v[819] := 0;v[820] := 0;v[821] := 0;v[822] := 0;v[823] := 0;v[824] := 0;v[825] := 0;v[826] := 0;v[827] := 0;v[828] := 0;v[829] := 0;v[830] := 0;v[831] := 0;v[832] := 0;v[833] := 0;v[834] := 0;v[835] := 0;v[836] := 0;v[837] := 0;v[838] := 0;v[839] := 0;v[840] := 0;v[841] := 0;v[842] := 0;v[843] := 0;v[844] := 0;v[845] := 0;v[846] := 0;v[847] := 0;v[848] := 0;v[849] := 0;v[850] := 0;v[851] := 0;v[852] := 0;v[853] := 0;v[854] := 0;v[855] := 0;v[856] := 0;v[857] := 0;v[858] := 0;v[859] := 0;v[860] := 0;v[861] := 0;v[862] := 0;v[863] := 0;v[864] := 0;v[865] := 0;v[866] := 0;v[867] := 0;v[868] := 0;v[869] := 0;v[870] := 0;v[871] := 0;v[872] := 0;v[873] := 0;v[874] := 0;v[875] := 0;v[876] := 0;v[877] := 0;v[878] := 0;v[879] := 0;v[880] := 0;v[881] := 0;v[882] := 0;v[883] := 0;v[884] := 0;v[885] := 0;v[886] := 0;v[887] := 0;v[888] := 0;v[889] := 0;v[890] := 0;v[891] := 0;v[892] := 0;v[893] := 0;v[894] := 0;v[895] := 0;v[896] := 0;v[897] := 0;v[898] := 0;v[899] := 0;v[900] := 0;v[901] := 0;v[902] := 0;v[903] := 0;v[904] := 0;v[905] := 0;v[906] := 0;v[907] := 0;v[908] := 0;v[909] := 0;v[910] := 0;v[911] := 0;v[912] := 0;v[913] := 0;v[914] := 0;v[915] := 0;v[916] := 0;v[917] := 0;v[918] := 0;v[919] := 0;v[920] := 0;v[921] := 0;v[922] := 0;v[923] := 0;v[924] := 0;v[925] := 0;v[926] := 0;v[927] := 0;v[928] := 0;v[929] := 0;v[930] := 0;v[931] := 0;v[932] := 0;v[933] := 0;v[934] := 0;v[935] := 0;v[936] := 0;v[937] := 0;v[938] := 0;v[939] := 0;v[940] := 0;v[941] := 0;v[942] := 0;v[943] := 0;v[944] := 0;v[945] := 0;v[946] := 0;v[947] := 0;v[948] := 0;v[949] := 0;v[950] := 0;v[951] := 0;v[952] := 0;v[953] := 0;v[954] := 0;v[955] := 0;v[956] := 0;v[957] := 0;v[958] := 0;v[959] := 0;v[960] := 0;v[961] := 0;v[962] := 0;v[963] := 0;v[964] := 0;v[965] := 0;v[966] := 0;v[967] := 0;v[968] := 0;v[969] := 0;v[970] := 0;v[971] := 0;v[972] := 0;v[973] := 0;v[974] := 0;v[975] := 0;v[976] := 0;v[977] := 0;v[978] := 0;v[979] := 0;v[980] := 0;v[981] := 0;v[982] := 0;v[983] := 0;v[984] := 0;v[985] := 0;v[986] := 0;v[987] := 0;v[988] := 0;v[989] := 0;v[990] := 0;v[991] := 0;v[992] := 0;v[993] := 0;v[994] := 0;v[995] := 0;v[996] := 0;v[997] := 0;v[998] := 0;v[999] := 0;v[1000] := 0;v[1001] := 0;v[1002] := 0;v[1003] := 0;v[1004] := 0;v[1005] := 0;v[1006] := 0;v[1007] := 0;v[1008] := 0;v[1009] := 0;v[1010] := 0;v[1011] := 0;v[1012] := 0;v[1013] := 0;v[1014] := 0;v[1015] := 0;v[1016] := 0;v[1017] := 0;v[1018] := 0;v[1019] := 0;v[1020] := 0;v[1021] := 0;v[1022] := 0;v[1023] := 0;v[1024] := 0;v[1025] := 0;v[1026] := 0;v[1027] := 0;v[1028] := 0;v[1029] := 0;v[1030] := 0;v[1031] := 0;v[1032] := 0;v[1033] := 0;v[1034] := 0;v[1035] := 0;v[1036] := 0;v[1037] := 0;v[1038] := 0;v[1039] := 0;v[1040] := 0;v[1041] := 0;v[1042] := 0;v[1043] := 0;v[1044] := 0;v[1045] := 0;v[1046] := 0;v[1047] := 0;v[1048] := 0;v[1049] := 0;v[1050] := 0;v[1051] := 0;v[1052] := 0;v[1053] := 0;v[1054] := 0;v[1055] := 0;v[1056] := 0;v[1057] := 0;v[1058] := 0;v[1059] := 0;v[1060] := 0;v[1061] := 0;v[1062] := 0;v[1063] := 0;v[1064] := 0;v[1065] := 0;v[1066] := 0;v[1067] := 0;v[1068] := 0;v[1069] := 0;v[1070] := 0;v[1071] := 0;v[1072] := 0;v[1073] := 0;v[1074] := 0;v[1075] := 0;v[1076] := 0;v[1077] := 0;v[1078] := 0;v[1079] := 0;v[1080] := 0;v[1081] := 0;v[1082] := 0;v[1083] := 0;v[1084] := 0;v[1085] := 0;v[1086] := 0;v[1087] := 0;v[1088] := 0;v[1089] := 0;v[1090] := 0;v[1091] := 0;v[1092] := 0;v[1093] := 0;v[1094] := 0;v[1095] := 0;v[1096] := 0;v[1097] := 0;v[1098] := 0;v[1099] := 0;v[1100] := 0;v[1101] := 0;v[1102] := 0;v[1103] := 0;v[1104] := 0;v[1105] := 0;v[1106] := 0;v[1107] := 0;v[1108] := 0;v[1109] := 0;v[1110] := 0;v[1111] := 0;v[1112] := 0;v[1113] := 0;v[1114] := 0;v[1115] := 0;v[1116] := 0;v[1117] := 0;v[1118] := 0;v[1119] := 0;v[1120] := 0;v[1121] := 0;v[1122] := 0;v[1123] := 0;v[1124] := 0;v[1125] := 0;v[1126] := 0;v[1127] := 0;v[1128] := 0;v[1129] := 0;v[1130] := 0;v[1131] := 0;v[1132] := 0;v[1133] := 0;v[1134] := 0;v[1135] := 0;v[1136] := 0;v[1137] := 0;v[1138] := 0;v[1139] := 0;v[1140] := 0;v[1141] := 0;v[1142] := 0;v[1143] := 0;v[1144] := 0;v[1145] := 0;v[1146] := 0;v[1147] := 0;v[1148] := 0;v[1149] := 0;v[1150] := 0;v[1151] := 0;v[1152] := 0;v[1153] := 0;v[1154] := 0;v[1155] := 0;v[1156] := 0;v[1157] := 0;v[1158] := 0;v[1159] := 0;v[1160] := 0;v[1161] := 0;v[1162] := 0;v[1163] := 0;v[1164] := 0;v[1165] := 0;v[1166] := 0;v[1167] := 0;v[1168] := 0;v[1169] := 0;v[1170] := 0;v[1171] := 0;v[1172] := 0;v[1173] := 0;v[1174] := 0;v[1175] := 0;v[1176] := 0;v[1177] := 0;v[1178] := 0;v[1179] := 0;v[1180] := 0;v[1181] := 0;v[1182] := 0;v[1183] := 0;v[1184] := 0;v[1185] := 0;v[1186] := 0;v[1187] := 0;v[1188] := 0;v[1189] := 0;v[1190] := 0;v[1191] := 0;v[1192] := 0;v[1193] := 0;v[1194] := 0;v[1195] := 0;v[1196] := 0;v[1197] := 0;v[1198] := 0;v[1199] := 0;v[1200] := 0;v[1201] := 0;v[1202] := 0;v[1203] := 0;v[1204] := 0;v[1205] := 0;v[1206] := 0;v[1207] := 0;v[1208] := 0;v[1209] := 0;v[1210] := 0;v[1211] := 0;v[1212] := 0;v[1213] := 0;v[1214] := 0;v[1215] := 0;v[1216] := 0;v[1217] := 0;v[1218] := 0;v[1219] := 0;v[1220] := 0;v[1221] := 0;v[1222] := 0;v[1223] := 0;v[1224] := 0;v[1225] := 0;v[1226] := 0;v[1227] := 0;v[1228] := 0;v[1229] := 0;v[1230] := 0;v[1231] := 0;v[1232] := 0;v[1233] := 0;v[1234] := 0;v[1235] := 0;v[1236] := 0;v[1237] := 0;v[1238] := 0;v[1239] := 0;v[1240] := 0;v[1241] := 0;v[1242] := 0;v[1243] := 0;v[1244] := 0;v[1245] := 0;v[1246] := 0;v[1247] := 0;v[1248] := 0;v[1249] := 0;v[1250] := 0;v[1251] := 0;v[1252] := 0;v[1253] := 0;v[1254] := 0;v[1255] := 0;v[1256] := 0;v[1257] := 0;v[1258] := 0;v[1259] := 0;v[1260] := 0;v[1261] := 0;v[1262] := 0;v[1263] := 0;v[1264] := 0;v[1265] := 0;v[1266] := 0;v[1267] := 0;v[1268] := 0;v[1269] := 0;v[1270] := 0;v[1271] := 0;v[1272] := 0;v[1273] := 0;v[1274] := 0;v[1275] := 0;v[1276] := 0;v[1277] := 0;v[1278] := 0;v[1279] := 0;v[1280] := 0;v[1281] := 0;v[1282] := 0;v[1283] := 0;v[1284] := 0;v[1285] := 0;v[1286] := 0;v[1287] := 0;v[1288] := 0;v[1289] := 0;v[1290] := 0;v[1291] := 0;v[1292] := 0;v[1293] := 0;v[1294] := 0;v[1295] := 0;v[1296] := 0;v[1297] := 0;v[1298] := 0;v[1299] := 0;v[1300] := 0;v[1301] := 0;v[1302] := 0;v[1303] := 0;v[1304] := 0;v[1305] := 0;v[1306] := 0;v[1307] := 0;v[1308] := 0;v[1309] := 0;v[1310] := 0;v[1311] := 0;v[1312] := 0;v[1313] := 0;v[1314] := 0;v[1315] := 0;v[1316] := 0;v[1317] := 0;v[1318] := 0;v[1319] := 0;v[1320] := 0;v[1321] := 0;v[1322] := 0;v[1323] := 0;v[1324] := 0;v[1325] := 0;v[1326] := 0;v[1327] := 0;v[1328] := 0;v[1329] := 0;v[1330] := 0;v[1331] := 0;v[1332] := 0;v[1333] := 0;v[1334] := 0;v[1335] := 0;v[1336] := 0;v[1337] := 0;v[1338] := 0;v[1339] := 0;v[1340] := 0;v[1341] := 0;v[1342] := 0;v[1343] := 0;v[1344] := 0;v[1345] := 0;v[1346] := 0;v[1347] := 0;v[1348] := 0;v[1349] := 0;v[1350] := 0;v[1351] := 0;v[1352] := 0;v[1353] := 0;v[1354] := 0;v[1355] := 0;v[1356] := 0;v[1357] := 0;v[1358] := 0;v[1359] := 0;v[1360] := 0;v[1361] := 0;v[1362] := 0;v[1363] := 0;v[1364] := 0;v[1365] := 0;v[1366] := 0;v[1367] := 0;v[1368] := 0;v[1369] := 0;v[1370] := 0;v[1371] := 0;v[1372] := 0;v[1373] := 0;v[1374] := 0;v[1375] := 0;v[1376] := 0;v[1377] := 0;v[1378] := 0;v[1379] := 0;v[1380] := 0;v[1381] := 0;v[1382] := 0;v[1383] := 0;v[1384] := 0;v[1385] := 0;v[1386] := 0;v[1387] := 0;v[1388] := 0;v[1389] := 0;v[1390] := 0;v[1391] := 0;v[1392] := 0;v[1393] := 0;v[1394] := 0;v[1395] := 0;v[1396] := 0;v[1397] := 0;v[1398] := 0;v[1399] := 0;v[1400] := 0;v[1401] := 0;v[1402] := 0;v[1403] := 0;v[1404] := 0;v[1405] := 0;v[1406] := 0;v[1407] := 0;v[1408] := 0;v[1409] := 0;v[1410] := 0;v[1411] := 0;v[1412] := 0;v[1413] := 0;v[1414] := 0;v[1415] := 0;v[1416] := 0;v[1417] := 0;v[1418] := 0;v[1419] := 0;v[1420] := 0;v[1421] := 0;v[1422] := 0;v[1423] := 0;v[1424] := 0;v[1425] := 0;v[1426] := 0;v[1427] := 0;v[1428] := 0;v[1429] := 0;v[1430] := 0;v[1431] := 0;v[1432] := 0;v[1433] := 0;v[1434] := 0;v[1435] := 0;v[1436] := 0;v[1437] := 0;v[1438] := 0;v[1439] := 0;v[1440] := 0;v[1441] := 0;v[1442] := 0;v[1443] := 0;v[1444] := 0;v[1445] := 0;v[1446] := 0;v[1447] := 0;v[1448] := 0;v[1449] := 0;v[1450] := 0;v[1451] := 0;v[1452] := 0;v[1453] := 0;v[1454] := 0;v[1455] := 0;v[1456] := 0;v[1457] := 0;v[1458] := 0;v[1459] := 0;v[1460] := 0;v[1461] := 0;v[1462] := 0;v[1463] := 0;v[1464] := 0;v[1465] := 0;v[1466] := 0;v[1467] := 0;v[1468] := 0;v[1469] := 0;v[1470] := 0;v[1471] := 0;v[1472] := 0;v[1473] := 0;v[1474] := 0;v[1475] := 0;v[1476] := 0;v[1477] := 0;v[1478] := 0;v[1479] := 0;v[1480] := 0;v[1481] := 0;v[1482] := 0;v[1483] := 0;v[1484] := 0;v[1485] := 0;v[1486] := 0;v[1487] := 0;v[1488] := 0;v[1489] := 0;v[1490] := 0;v[1491] := 0;v[1492] := 0;v[1493] := 0;v[1494] := 0;v[1495] := 0;v[1496] := 0;v[1497] := 0;v[1498] := 0;v[1499] := 0;v[1500] := 0;v[1501] := 0;v[1502] := 0;v[1503] := 0;v[1504] := 0;v[1505] := 0;v[1506] := 0;v[1507] := 0;v[1508] := 0;v[1509] := 0;v[1510] := 0;v[1511] := 0;v[1512] := 0;v[1513] := 0;v[1514] := 0;v[1515] := 0;v[1516] := 0;v[1517] := 0;v[1518] := 0;v[1519] := 0;v[1520] := 0;v[1521] := 0;v[1522] := 0;v[1523] := 0;v[1524] := 0;v[1525] := 0;v[1526] := 0;v[1527] := 0;v[1528] := 0;v[1529] := 0;v[1530] := 0;v[1531] := 0;v[1532] := 0;v[1533] := 0;v[1534] := 0;v[1535] := 0;v[1536] := 0;v[1537] := 0;v[1538] := 0;v[1539] := 0;v[1540] := 0;v[1541] := 0;v[1542] := 0;v[1543] := 0;v[1544] := 0;v[1545] := 0;v[1546] := 0;v[1547] := 0;v[1548] := 0;v[1549] := 0;v[1550] := 0;v[1551] := 0;v[1552] := 0;v[1553] := 0;v[1554] := 0;v[1555] := 0;v[1556] := 0;v[1557] := 0;v[1558] := 0;v[1559] := 0;v[1560] := 0;v[1561] := 0;v[1562] := 0;v[1563] := 0;v[1564] := 0;v[1565] := 0;v[1566] := 0;v[1567] := 0;v[1568] := 0;v[1569] := 0;v[1570] := 0;v[1571] := 0;v[1572] := 0;v[1573] := 0;v[1574] := 0;v[1575] := 0;v[1576] := 0;v[1577] := 0;v[1578] := 0;v[1579] := 0;v[1580] := 0;v[1581] := 0;v[1582] := 0;v[1583] := 0;v[1584] := 0;v[1585] := 0;v[1586] := 0;v[1587] := 0;v[1588] := 0;v[1589] := 0;v[1590] := 0;v[1591] := 0;v[1592] := 0;v[1593] := 0;v[1594] := 0;v[1595] := 0;v[1596] := 0;v[1597] := 0;v[1598] := 0;v[1599] := 0;v[1600] := 0;v[1601] := 0;v[1602] := 0;v[1603] := 0;v[1604] := 0;v[1605] := 0;v[1606] := 0;v[1607] := 0;v[1608] := 0;v[1609] := 0;v[1610] := 0;v[1611] := 0;v[1612] := 0;v[1613] := 0;v[1614] := 0;v[1615] := 0;v[1616] := 0;v[1617] := 0;v[1618] := 0;v[1619] := 0;v[1620] := 0;v[1621] := 0;v[1622] := 0;v[1623] := 0;v[1624] := 0;v[1625] := 0;v[1626] := 0;v[1627] := 0;v[1628] := 0;v[1629] := 0;v[1630] := 0;v[1631] := 0;v[1632] := 0;v[1633] := 0;v[1634] := 0;v[1635] := 0;v[1636] := 0;v[1637] := 0;v[1638] := 0;v[1639] := 0;v[1640] := 0;v[1641] := 0;v[1642] := 0;v[1643] := 0;v[1644] := 0;v[1645] := 0;v[1646] := 0;v[1647] := 0;v[1648] := 0;v[1649] := 0;v[1650] := 0;v[1651] := 0;v[1652] := 0;v[1653] := 0;v[1654] := 0;v[1655] := 0;v[1656] := 0;v[1657] := 0;v[1658] := 0;v[1659] := 0;v[1660] := 0;v[1661] := 0;v[1662] := 0;v[1663] := 0;v[1664] := 0;v[1665] := 0;v[1666] := 0;v[1667] := 0;v[1668] := 0;v[1669] := 0;v[1670] := 0;v[1671] := 0;v[1672] := 0;v[1673] := 0;v[1674] := 0;v[1675] := 0;v[1676] := 0;v[1677] := 0;v[1678] := 0;v[1679] := 0;v[1680] := 0;v[1681] := 0;v[1682] := 0;v[1683] := 0;v[1684] := 0;v[1685] := 0;v[1686] := 0;v[1687] := 0;v[1688] := 0;v[1689] := 0;v[1690] := 0;v[1691] := 0;v[1692] := 0;v[1693] := 0;v[1694] := 0;v[1695] := 0;v[1696] := 0;v[1697] := 0;v[1698] := 0;v[1699] := 0;v[1700] := 0;v[1701] := 0;v[1702] := 0;v[1703] := 0;v[1704] := 0;v[1705] := 0;v[1706] := 0;v[1707] := 0;v[1708] := 0;v[1709] := 0;v[1710] := 0;v[1711] := 0;v[1712] := 0;v[1713] := 0;v[1714] := 0;v[1715] := 0;v[1716] := 0;v[1717] := 0;v[1718] := 0;v[1719] := 0;v[1720] := 0;v[1721] := 0;v[1722] := 0;v[1723] := 0;v[1724] := 0;v[1725] := 0;v[1726] := 0;v[1727] := 0;v[1728] := 0;v[1729] := 0;v[1730] := 0;v[1731] := 0;v[1732] := 0;v[1733] := 0;v[1734] := 0;v[1735] := 0;v[1736] := 0;v[1737] := 0;v[1738] := 0;v[1739] := 0;v[1740] := 0;v[1741] := 0;v[1742] := 0;v[1743] := 0;v[1744] := 0;v[1745] := 0;v[1746] := 0;v[1747] := 0;v[1748] := 0;v[1749] := 0;v[1750] := 0;v[1751] := 0;v[1752] := 0;v[1753] := 0;v[1754] := 0;v[1755] := 0;v[1756] := 0;v[1757] := 0;v[1758] := 0;v[1759] := 0;v[1760] := 0;v[1761] := 0;v[1762] := 0;v[1763] := 0;v[1764] := 0;v[1765] := 0;v[1766] := 0;v[1767] := 0;v[1768] := 0;v[1769] := 0;v[1770] := 0;v[1771] := 0;v[1772] := 0;v[1773] := 0;v[1774] := 0;v[1775] := 0;v[1776] := 0;v[1777] := 0;v[1778] := 0;v[1779] := 0;v[1780] := 0;v[1781] := 0;v[1782] := 0;v[1783] := 0;v[1784] := 0;v[1785] := 0;v[1786] := 0;v[1787] := 0;v[1788] := 0;v[1789] := 0;v[1790] := 0;v[1791] := 0;v[1792] := 0;v[1793] := 0;v[1794] := 0;v[1795] := 0;v[1796] := 0;v[1797] := 0;v[1798] := 0;v[1799] := 0;v[1800] := 0;v[1801] := 0;v[1802] := 0;v[1803] := 0;v[1804] := 0;v[1805] := 0;v[1806] := 0;v[1807] := 0;v[1808] := 0;v[1809] := 0;v[1810] := 0;v[1811] := 0;v[1812] := 0;v[1813] := 0;v[1814] := 0;v[1815] := 0;v[1816] := 0;v[1817] := 0;v[1818] := 0;v[1819] := 0;v[1820] := 0;v[1821] := 0;v[1822] := 0;v[1823] := 0;v[1824] := 0;v[1825] := 0;v[1826] := 0;v[1827] := 0;v[1828] := 0;v[1829] := 0;v[1830] := 0;v[1831] := 0;v[1832] := 0;v[1833] := 0;v[1834] := 0;v[1835] := 0;v[1836] := 0;v[1837] := 0;v[1838] := 0;v[1839] := 0;v[1840] := 0;v[1841] := 0;v[1842] := 0;v[1843] := 0;v[1844] := 0;v[1845] := 0;v[1846] := 0;v[1847] := 0;v[1848] := 0;v[1849] := 0;v[1850] := 0;v[1851] := 0;v[1852] := 0;v[1853] := 0;v[1854] := 0;v[1855] := 0;v[1856] := 0;v[1857] := 0;v[1858] := 0;v[1859] := 0;v[1860] := 0;v[1861] := 0;v[1862] := 0;v[1863] := 0;v[1864] := 0;v[1865] := 0;v[1866] := 0;v[1867] := 0;v[1868] := 0;v[1869] := 0;v[1870] := 0;v[1871] := 0;v[1872] := 0;v[1873] := 0;v[1874] := 0;v[1875] := 0;v[1876] := 0;v[1877] := 0;v[1878] := 0;v[1879] := 0;v[1880] := 0;v[1881] := 0;v[1882] := 0;v[1883] := 0;v[1884] := 0;v[1885] := 0;v[1886] := 0;v[1887] := 0;v[1888] := 0;v[1889] := 0;v[1890] := 0;v[1891] := 0;v[1892] := 0;v[1893] := 0;v[1894] := 0;v[1895] := 0;v[1896] := 0;v[1897] := 0;v[1898] := 0;v[1899] := 0;v[1900] := 0;v[1901] := 0;v[1902] := 0;v[1903] := 0;v[1904] := 0;v[1905] := 0;v[1906] := 0;v[1907] := 0;v[1908] := 0;v[1909] := 0;v[1910] := 0;v[1911] := 0;v[1912] := 0;v[1913] := 0;v[1914] := 0;v[1915] := 0;v[1916] := 0;v[1917] := 0;v[1918] := 0;v[1919] := 0;v[1920] := 0;v[1921] := 0;v[1922] := 0;v[1923] := 0;v[1924] := 0;v[1925] := 0;v[1926] := 0;v[1927] := 0;v[1928] := 0;v[1929] := 0;v[1930] := 0;v[1931] := 0;v[1932] := 0;v[1933] := 0;v[1934] := 0;v[1935] := 0;v[1936] := 0;v[1937] := 0;v[1938] := 0;v[1939] := 0;v[1940] := 0;v[1941] := 0;v[1942] := 0;v[1943] := 0;v[1944] := 0;v[1945] := 0;v[1946] := 0;v[1947] := 0;v[1948] := 0;v[1949] := 0;v[1950] := 0;v[1951] := 0;v[1952] := 0;v[1953] := 0;v[1954] := 0;v[1955] := 0;v[1956] := 0;v[1957] := 0;v[1958] := 0;v[1959] := 0;v[1960] := 0;v[1961] := 0;v[1962] := 0;v[1963] := 0;v[1964] := 0;v[1965] := 0;v[1966] := 0;v[1967] := 0;v[1968] := 0;v[1969] := 0;v[1970] := 0;v[1971] := 0;v[1972] := 0;v[1973] := 0;v[1974] := 0;v[1975] := 0;v[1976] := 0;v[1977] := 0;v[1978] := 0;v[1979] := 0;v[1980] := 0;v[1981] := 0;v[1982] := 0;v[1983] := 0;v[1984] := 0;v[1985] := 0;v[1986] := 0;v[1987] := 0;v[1988] := 0;v[1989] := 0;v[1990] := 0;v[1991] := 0;v[1992] := 0;v[1993] := 0;v[1994] := 0;v[1995] := 0;v[1996] := 0;v[1997] := 0;v[1998] := 0;v[1999] := 0;v[2000] := 0;v[2001] := 0;v[2002] := 0;v[2003] := 0;v[2004] := 0;v[2005] := 0;v[2006] := 0;v[2007] := 0;v[2008] := 0;v[2009] := 0;v[2010] := 0;v[2011] := 0;v[2012] := 0;v[2013] := 0;v[2014] := 0;v[2015] := 0;v[2016] := 0;v[2017] := 0;v[2018] := 0;v[2019] := 0;v[2020] := 0;v[2021] := 0;v[2022] := 0;v[2023] := 0;v[2024] := 0;v[2025] := 0;v[2026] := 0;v[2027] := 0;v[2028] := 0;v[2029] := 0;v[2030] := 0;v[2031] := 0;v[2032] := 0;v[2033] := 0;v[2034] := 0;v[2035] := 0;v[2036] := 0;v[2037] := 0;v[2038] := 0;v[2039] := 0;v[2040] := 0;v[2041] := 0;v[2042] := 0;v[2043] := 0;v[2044] := 0;v[2045] := 0;v[2046] := 0;v[2047] := 0;v[2048] := 0;v[2049] := 0;v[2050] := 0;v[2051] := 0;v[2052] := 0;v[2053] := 0;v[2054] := 0;v[2055] := 0;v[2056] := 0;v[2057] := 0;v[2058] := 0;v[2059] := 0;v[2060] := 0;v[2061] := 0;v[2062] := 0;v[2063] := 0;v[2064] := 0;v[2065] := 0;v[2066] := 0;v[2067] := 0;v[2068] := 0;v[2069] := 0;v[2070] := 0;v[2071] := 0;v[2072] := 0;v[2073] := 0;v[2074] := 0;v[2075] := 0;v[2076] := 0;v[2077] := 0;v[2078] := 0;v[2079] := 0;v[2080] := 0;v[2081] := 0;v[2082] := 0;v[2083] := 0;v[2084] := 0;v[2085] := 0;v[2086] := 0;v[2087] := 0;v[2088] := 0;v[2089] := 0;v[2090] := 0;v[2091] := 0;v[2092] := 0;v[2093] := 0;v[2094] := 0;v[2095] := 0;v[2096] := 0;v[2097] := 0;v[2098] := 0;v[2099] := 0;v[2100] := 0;v[2101] := 0;v[2102] := 0;v[2103] := 0;v[2104] := 0;v[2105] := 0;v[2106] := 0;v[2107] := 0;v[2108] := 0;v[2109] := 0;v[2110] := 0;v[2111] := 0;v[2112] := 0;v[2113] := 0;v[2114] := 0;v[2115] := 0;v[2116] := 0;v[2117] := 0;v[2118] := 0;v[2119] := 0;v[2120] := 0;v[2121] := 0;v[2122] := 0;v[2123] := 0;v[2124] := 0;v[2125] := 0;v[2126] := 0;v[2127] := 0;v[2128] := 0;v[2129] := 0;v[2130] := 0;v[2131] := 0;v[2132] := 0;v[2133] := 0;v[2134] := 0;v[2135] := 0;v[2136] := 0;v[2137] := 0;v[2138] := 0;v[2139] := 0;v[2140] := 0;v[2141] := 0;v[2142] := 0;v[2143] := 0;v[2144] := 0;v[2145] := 0;v[2146] := 0;v[2147] := 0;v[2148] := 0;v[2149] := 0;v[2150] := 0;v[2151] := 0;v[2152] := 0;v[2153] := 0;v[2154] := 0;v[2155] := 0;v[2156] := 0;v[2157] := 0;v[2158] := 0;v[2159] := 0;v[2160] := 0;v[2161] := 0;v[2162] := 0;v[2163] := 0;v[2164] := 0;v[2165] := 0;v[2166] := 0;v[2167] := 0;v[2168] := 0;v[2169] := 0;v[2170] := 0;v[2171] := 0;v[2172] := 0;v[2173] := 0;v[2174] := 0;v[2175] := 0;v[2176] := 0;v[2177] := 0;v[2178] := 0;v[2179] := 0;v[2180] := 0;v[2181] := 0;v[2182] := 0;v[2183] := 0;v[2184] := 0;v[2185] := 0;v[2186] := 0;v[2187] := 0;v[2188] := 0;v[2189] := 0;v[2190] := 0;v[2191] := 0;v[2192] := 0;v[2193] := 0;v[2194] := 0;v[2195] := 0;v[2196] := 0;v[2197] := 0;v[2198] := 0;v[2199] := 0;v[2200] := 0;v[2201] := 0;v[2202] := 0;v[2203] := 0;v[2204] := 0;v[2205] := 0;v[2206] := 0;v[2207] := 0;v[2208] := 0;v[2209] := 0;v[2210] := 0;v[2211] := 0;v[2212] := 0;v[2213] := 0;v[2214] := 0;v[2215] := 0;v[2216] := 0;v[2217] := 0;v[2218] := 0;v[2219] := 0;v[2220] := 0;v[2221] := 0;v[2222] := 0;v[2223] := 0;v[2224] := 0;v[2225] := 0;v[2226] := 0;v[2227] := 0;v[2228] := 0;v[2229] := 0;v[2230] := 0;v[2231] := 0;v[2232] := 0;v[2233] := 0;v[2234] := 0;v[2235] := 0;v[2236] := 0;v[2237] := 0;v[2238] := 0;v[2239] := 0;v[2240] := 0;v[2241] := 0;v[2242] := 0;v[2243] := 0;v[2244] := 0;v[2245] := 0;v[2246] := 0;v[2247] := 0;v[2248] := 0;v[2249] := 0;v[2250] := 0;v[2251] := 0;v[2252] := 0;v[2253] := 0;v[2254] := 0;v[2255] := 0;v[2256] := 0;v[2257] := 0;v[2258] := 0;v[2259] := 0;v[2260] := 0;v[2261] := 0;v[2262] := 0;v[2263] := 0;v[2264] := 0;v[2265] := 0;v[2266] := 0;v[2267] := 0;v[2268] := 0;v[2269] := 0;v[2270] := 0;v[2271] := 0;v[2272] := 0;v[2273] := 0;v[2274] := 0;v[2275] := 0;v[2276] := 0;v[2277] := 0;v[2278] := 0;v[2279] := 0;v[2280] := 0;v[2281] := 0;v[2282] := 0;v[2283] := 0;v[2284] := 0;v[2285] := 0;v[2286] := 0;v[2287] := 0;v[2288] := 0;v[2289] := 0;v[2290] := 0;v[2291] := 0;v[2292] := 0;v[2293] := 0;v[2294] := 0;v[2295] := 0;v[2296] := 0;v[2297] := 0;v[2298] := 0;v[2299] := 0;v[2300] := 0;v[2301] := 0;v[2302] := 0;v[2303] := 0;v[2304] := 0;v[2305] := 0;v[2306] := 0;v[2307] := 0;v[2308] := 0;v[2309] := 0;v[2310] := 0;v[2311] := 0;v[2312] := 0;v[2313] := 0;v[2314] := 0;v[2315] := 0;v[2316] := 0;v[2317] := 0;v[2318] := 0;v[2319] := 0;v[2320] := 0;v[2321] := 0;v[2322] := 0;v[2323] := 0;v[2324] := 0;v[2325] := 0;v[2326] := 0;v[2327] := 0;v[2328] := 0;v[2329] := 0;v[2330] := 0;v[2331] := 0;v[2332] := 0;v[2333] := 0;v[2334] := 0;v[2335] := 0;v[2336] := 0;v[2337] := 0;v[2338] := 0;v[2339] := 0;v[2340] := 0;v[2341] := 0;v[2342] := 0;v[2343] := 0;v[2344] := 0;v[2345] := 0;v[2346] := 0;v[2347] := 0;v[2348] := 0;v[2349] := 0;v[2350] := 0;v[2351] := 0;v[2352] := 0;v[2353] := 0;v[2354] := 0;v[2355] := 0;v[2356] := 0;v[2357] := 0;v[2358] := 0;v[2359] := 0;v[2360] := 0;v[2361] := 0;v[2362] := 0;v[2363] := 0;v[2364] := 0;v[2365] := 0;v[2366] := 0;v[2367] := 0;v[2368] := 0;v[2369] := 0;v[2370] := 0;v[2371] := 0;v[2372] := 0;v[2373] := 0;v[2374] := 0;v[2375] := 0;v[2376] := 0;v[2377] := 0;v[2378] := 0;v[2379] := 0;v[2380] := 0;v[2381] := 0;v[2382] := 0;v[2383] := 0;v[2384] := 0;v[2385] := 0;v[2386] := 0;v[2387] := 0;v[2388] := 0;v[2389] := 0;v[2390] := 0;v[2391] := 0;v[2392] := 0;v[2393] := 0;v[2394] := 0;v[2395] := 0;v[2396] := 0;v[2397] := 0;v[2398] := 0;v[2399] := 0;v[2400] := 0;v[2401] := 0;v[2402] := 0;v[2403] := 0;v[2404] := 0;v[2405] := 0;v[2406] := 0;v[2407] := 0;v[2408] := 0;v[2409] := 0;v[2410] := 0;v[2411] := 0;v[2412] := 0;v[2413] := 0;v[2414] := 0;v[2415] := 0;v[2416] := 0;v[2417] := 0;v[2418] := 0;v[2419] := 0;v[2420] := 0;v[2421] := 0;v[2422] := 0;v[2423] := 0;v[2424] := 0;v[2425] := 0;v[2426] := 0;v[2427] := 0;v[2428] := 0;v[2429] := 0;v[2430] := 0;v[2431] := 0;v[2432] := 0;v[2433] := 0;v[2434] := 0;v[2435] := 0;v[2436] := 0;v[2437] := 0;v[2438] := 0;v[2439] := 0;v[2440] := 0;v[2441] := 0;v[2442] := 0;v[2443] := 0;v[2444] := 0;v[2445] := 0;v[2446] := 0;v[2447] := 0;v[2448] := 0;v[2449] := 0;v[2450] := 0;v[2451] := 0;v[2452] := 0;v[2453] := 0;v[2454] := 0;v[2455] := 0;v[2456] := 0;v[2457] := 0;v[2458] := 0;v[2459] := 0;v[2460] := 0;v[2461] := 0;v[2462] := 0;v[2463] := 0;v[2464] := 0;v[2465] := 0;v[2466] := 0;v[2467] := 0;v[2468] := 0;v[2469] := 0;v[2470] := 0;v[2471] := 0;v[2472] := 0;v[2473] := 0;v[2474] := 0;v[2475] := 0;v[2476] := 0;v[2477] := 0;v[2478] := 0;v[2479] := 0;v[2480] := 0;v[2481] := 0;v[2482] := 0;v[2483] := 0;v[2484] := 0;v[2485] := 0;v[2486] := 0;v[2487] := 0;v[2488] := 0;v[2489] := 0;v[2490] := 0;v[2491] := 0;v[2492] := 0;v[2493] := 0;v[2494] := 0;v[2495] := 0;v[2496] := 0;v[2497] := 0;v[2498] := 0;v[2499] := 0;v[2500] := 0;v[2501] := 0;v[2502] := 0;v[2503] := 0;v[2504] := 0;v[2505] := 0;v[2506] := 0;v[2507] := 0;v[2508] := 0;v[2509] := 0;v[2510] := 0;v[2511] := 0;v[2512] := 0;v[2513] := 0;v[2514] := 0;v[2515] := 0;v[2516] := 0;v[2517] := 0;v[2518] := 0;v[2519] := 0;v[2520] := 0;v[2521] := 0;v[2522] := 0;v[2523] := 0;v[2524] := 0;v[2525] := 0;v[2526] := 0;v[2527] := 0;v[2528] := 0;v[2529] := 0;v[2530] := 0;v[2531] := 0;v[2532] := 0;v[2533] := 0;v[2534] := 0;v[2535] := 0;v[2536] := 0;v[2537] := 0;v[2538] := 0;v[2539] := 0;v[2540] := 0;v[2541] := 0;v[2542] := 0;v[2543] := 0;v[2544] := 0;v[2545] := 0;v[2546] := 0;v[2547] := 0;v[2548] := 0;v[2549] := 0;v[2550] := 0;v[2551] := 0;v[2552] := 0;v[2553] := 0;v[2554] := 0;v[2555] := 0;v[2556] := 0;v[2557] := 0;v[2558] := 0;v[2559] := 0;v[2560] := 0;v[2561] := 0;v[2562] := 0;v[2563] := 0;v[2564] := 0;v[2565] := 0;v[2566] := 0;v[2567] := 0;v[2568] := 0;v[2569] := 0;v[2570] := 0;v[2571] := 0;v[2572] := 0;v[2573] := 0;v[2574] := 0;v[2575] := 0;v[2576] := 0;v[2577] := 0;v[2578] := 0;v[2579] := 0;v[2580] := 0;v[2581] := 0;v[2582] := 0;v[2583] := 0;v[2584] := 0;v[2585] := 0;v[2586] := 0;v[2587] := 0;v[2588] := 0;v[2589] := 0;v[2590] := 0;v[2591] := 0;v[2592] := 0;v[2593] := 0;v[2594] := 0;v[2595] := 0;v[2596] := 0;v[2597] := 0;v[2598] := 0;v[2599] := 0;v[2600] := 0;v[2601] := 0;v[2602] := 0;v[2603] := 0;v[2604] := 0;v[2605] := 0;v[2606] := 0;v[2607] := 0;v[2608] := 0;v[2609] := 0;v[2610] := 0;v[2611] := 0;v[2612] := 0;v[2613] := 0;v[2614] := 0;v[2615] := 0;v[2616] := 0;v[2617] := 0;v[2618] := 0;v[2619] := 0;v[2620] := 0;v[2621] := 0;v[2622] := 0;v[2623] := 0;v[2624] := 0;v[2625] := 0;v[2626] := 0;v[2627] := 0;v[2628] := 0;v[2629] := 0;v[2630] := 0;v[2631] := 0;v[2632] := 0;v[2633] := 0;v[2634] := 0;v[2635] := 0;v[2636] := 0;v[2637] := 0;v[2638] := 0;v[2639] := 0;v[2640] := 0;v[2641] := 0;v[2642] := 0;v[2643] := 0;v[2644] := 0;v[2645] := 0;v[2646] := 0;v[2647] := 0;v[2648] := 0;v[2649] := 0;v[2650] := 0;v[2651] := 0;v[2652] := 0;v[2653] := 0;v[2654] := 0;v[2655] := 0;v[2656] := 0;v[2657] := 0;v[2658] := 0;v[2659] := 0;v[2660] := 0;v[2661] := 0;v[2662] := 0;v[2663] := 0;v[2664] := 0;v[2665] := 0;v[2666] := 0;v[2667] := 0;v[2668] := 0;v[2669] := 0;v[2670] := 0;v[2671] := 0;v[2672] := 0;v[2673] := 0;v[2674] := 0;v[2675] := 0;v[2676] := 0;v[2677] := 0;v[2678] := 0;v[2679] := 0;v[2680] := 0;v[2681] := 0;v[2682] := 0;v[2683] := 0;v[2684] := 0;v[2685] := 0;v[2686] := 0;v[2687] := 0;v[2688] := 0;v[2689] := 0;v[2690] := 0;v[2691] := 0;v[2692] := 0;v[2693] := 0;v[2694] := 0;v[2695] := 0;v[2696] := 0;v[2697] := 0;v[2698] := 0;v[2699] := 0;v[2700] := 0;v[2701] := 0;v[2702] := 0;v[2703] := 0;v[2704] := 0;v[2705] := 0;v[2706] := 0;v[2707] := 0;v[2708] := 0;v[2709] := 0;v[2710] := 0;v[2711] := 0;v[2712] := 0;v[2713] := 0;v[2714] := 0;v[2715] := 0;v[2716] := 0;v[2717] := 0;v[2718] := 0;v[2719] := 0;v[2720] := 0;v[2721] := 0;v[2722] := 0;v[2723] := 0;v[2724] := 0;v[2725] := 0;v[2726] := 0;v[2727] := 0;v[2728] := 0;v[2729] := 0;v[2730] := 0;v[2731] := 0;v[2732] := 0;v[2733] := 0;v[2734] := 0;v[2735] := 0;v[2736] := 0;v[2737] := 0;v[2738] := 0;v[2739] := 0;v[2740] := 0;v[2741] := 0;v[2742] := 0;v[2743] := 0;v[2744] := 0;v[2745] := 0;v[2746] := 0;v[2747] := 0;v[2748] := 0;v[2749] := 0;v[2750] := 0;v[2751] := 0;v[2752] := 0;v[2753] := 0;v[2754] := 0;v[2755] := 0;v[2756] := 0;v[2757] := 0;v[2758] := 0;v[2759] := 0;v[2760] := 0;v[2761] := 0;v[2762] := 0;v[2763] := 0;v[2764] := 0;v[2765] := 0;v[2766] := 0;v[2767] := 0;v[2768] := 0;v[2769] := 0;v[2770] := 0;v[2771] := 0;v[2772] := 0;v[2773] := 0;v[2774] := 0;v[2775] := 0;v[2776] := 0;v[2777] := 0;v[2778] := 0;v[2779] := 0;v[2780] := 0;v[2781] := 0;v[2782] := 0;v[2783] := 0;v[2784] := 0;v[2785] := 0;v[2786] := 0;v[2787] := 0;v[2788] := 0;v[2789] := 0;v[2790] := 0;v[2791] := 0;v[2792] := 0;v[2793] := 0;v[2794] := 0;v[2795] := 0;v[2796] := 0;v[2797] := 0;v[2798] := 0;v[2799] := 0;v[2800] := 0;v[2801] := 0;v[2802] := 0;v[2803] := 0;v[2804] := 0;v[2805] := 0;v[2806] := 0;v[2807] := 0;v[2808] := 0;v[2809] := 0;v[2810] := 0;v[2811] := 0;v[2812] := 0;v[2813] := 0;v[2814] := 0;v[2815] := 0;v[2816] := 0;v[2817] := 0;v[2818] := 0;v[2819] := 0;v[2820] := 0;v[2821] := 0;v[2822] := 0;v[2823] := 0;v[2824] := 0;v[2825] := 0;v[2826] := 0;v[2827] := 0;v[2828] := 0;v[2829] := 0;v[2830] := 0;v[2831] := 0;v[2832] := 0;v[2833] := 0;v[2834] := 0;v[2835] := 0;v[2836] := 0;v[2837] := 0;v[2838] := 0;v[2839] := 0;v[2840] := 0;v[2841] := 0;v[2842] := 0;v[2843] := 0;v[2844] := 0;v[2845] := 0;v[2846] := 0;v[2847] := 0;v[2848] := 0;v[2849] := 0;v[2850] := 0;v[2851] := 0;v[2852] := 0;v[2853] := 0;v[2854] := 0;v[2855] := 0;v[2856] := 0;v[2857] := 0;v[2858] := 0;v[2859] := 0;v[2860] := 0;v[2861] := 0;v[2862] := 0;v[2863] := 0;v[2864] := 0;v[2865] := 0;v[2866] := 0;v[2867] := 0;v[2868] := 0;v[2869] := 0;v[2870] := 0;v[2871] := 0;v[2872] := 0;v[2873] := 0;v[2874] := 0;v[2875] := 0;v[2876] := 0;v[2877] := 0;v[2878] := 0;v[2879] := 0;v[2880] := 0;v[2881] := 0;v[2882] := 0;v[2883] := 0;v[2884] := 0;v[2885] := 0;v[2886] := 0;v[2887] := 0;v[2888] := 0;v[2889] := 0;v[2890] := 0;v[2891] := 0;v[2892] := 0;v[2893] := 0;v[2894] := 0;v[2895] := 0;v[2896] := 0;v[2897] := 0;v[2898] := 0;v[2899] := 0;v[2900] := 0;v[2901] := 0;v[2902] := 0;v[2903] := 0;v[2904] := 0;v[2905] := 0;v[2906] := 0;v[2907] := 0;v[2908] := 0;v[2909] := 0;v[2910] := 0;v[2911] := 0;v[2912] := 0;v[2913] := 0;v[2914] := 0;v[2915] := 0;v[2916] := 0;v[2917] := 0;v[2918] := 0;v[2919] := 0;v[2920] := 0;v[2921] := 0;v[2922] := 0;v[2923] := 0;v[2924] := 0;v[2925] := 0;v[2926] := 0;v[2927] := 0;v[2928] := 0;v[2929] := 0;v[2930] := 0;v[2931] := 0;v[2932] := 0;v[2933] := 0;v[2934] := 0;v[2935] := 0;v[2936] := 0;v[2937] := 0;v[2938] := 0;v[2939] := 0;v[2940] := 0;v[2941] := 0;v[2942] := 0;v[2943] := 0;v[2944] := 0;v[2945] := 0;v[2946] := 0;v[2947] := 0;v[2948] := 0;v[2949] := 0;v[2950] := 0;v[2951] := 0;v[2952] := 0;v[2953] := 0;v[2954] := 0;v[2955] := 0;v[2956] := 0;v[2957] := 0;v[2958] := 0;v[2959] := 0;v[2960] := 0;v[2961] := 0;v[2962] := 0;v[2963] := 0;v[2964] := 0;v[2965] := 0;v[2966] := 0;v[2967] := 0;v[2968] := 0;v[2969] := 0;v[2970] := 0;v[2971] := 0;v[2972] := 0;v[2973] := 0;v[2974] := 0;v[2975] := 0;v[2976] := 0;v[2977] := 0;v[2978] := 0;v[2979] := 0;v[2980] := 0;v[2981] := 0;v[2982] := 0;v[2983] := 0;v[2984] := 0;v[2985] := 0;v[2986] := 0;v[2987] := 0;v[2988] := 0;v[2989] := 0;v[2990] := 0;v[2991] := 0;v[2992] := 0;v[2993] := 0;v[2994] := 0;v[2995] := 0;v[2996] := 0;v[2997] := 0;v[2998] := 0;v[2999] := 0;v[3000] := 0;v[3001] := 0;v[3002] := 0;v[3003] := 0;v[3004] := 0;v[3005] := 0;v[3006] := 0;v[3007] := 0;v[3008] := 0;v[3009] := 0;v[3010] := 0;v[3011] := 0;v[3012] := 0;v[3013] := 0;v[3014] := 0;v[3015] := 0;v[3016] := 0;v[3017] := 0;v[3018] := 0;v[3019] := 0;v[3020] := 0;v[3021] := 0;v[3022] := 0;v[3023] := 0;v[3024] := 0;v[3025] := 0;v[3026] := 0;v[3027] := 0;v[3028] := 0;v[3029] := 0;v[3030] := 0;v[3031] := 0;v[3032] := 0;v[3033] := 0;v[3034] := 0;v[3035] := 0;v[3036] := 0;v[3037] := 0;v[3038] := 0;v[3039] := 0;v[3040] := 0;v[3041] := 0;v[3042] := 0;v[3043] := 0;v[3044] := 0;v[3045] := 0;v[3046] := 0;v[3047] := 0;v[3048] := 0;v[3049] := 0;v[3050] := 0;v[3051] := 0;v[3052] := 0;v[3053] := 0;v[3054] := 0;v[3055] := 0;v[3056] := 0;v[3057] := 0;v[3058] := 0;v[3059] := 0;v[3060] := 0;v[3061] := 0;v[3062] := 0;v[3063] := 0;v[3064] := 0;v[3065] := 0;v[3066] := 0;v[3067] := 0;v[3068] := 0;v[3069] := 0;v[3070] := 0;v[3071] := 0;v[3072] := 0;v[3073] := 0;v[3074] := 0;v[3075] := 0;v[3076] := 0;v[3077] := 0;v[3078] := 0;v[3079] := 0;v[3080] := 0;v[3081] := 0;v[3082] := 0;v[3083] := 0;v[3084] := 0;v[3085] := 0;v[3086] := 0;v[3087] := 0;v[3088] := 0;v[3089] := 0;v[3090] := 0;v[3091] := 0;v[3092] := 0;v[3093] := 0;v[3094] := 0;v[3095] := 0;v[3096] := 0;v[3097] := 0;v[3098] := 0;v[3099] := 0;v[3100] := 0;v[3101] := 0;v[3102] := 0;v[3103] := 0;v[3104] := 0;v[3105] := 0;v[3106] := 0;v[3107] := 0;v[3108] := 0;v[3109] := 0;v[3110] := 0;v[3111] := 0;v[3112] := 0;v[3113] := 0;v[3114] := 0;v[3115] := 0;v[3116] := 0;v[3117] := 0;v[3118] := 0;v[3119] := 0;v[3120] := 0;v[3121] := 0;v[3122] := 0;v[3123] := 0;v[3124] := 0;v[3125] := 0;v[3126] := 0;v[3127] := 0;v[3128] := 0;v[3129] := 0;v[3130] := 0;v[3131] := 0;v[3132] := 0;v[3133] := 0;v[3134] := 0;v[3135] := 0;v[3136] := 0;v[3137] := 0;v[3138] := 0;v[3139] := 0;v[3140] := 0;v[3141] := 0;v[3142] := 0;v[3143] := 0;v[3144] := 0;v[3145] := 0;v[3146] := 0;v[3147] := 0;v[3148] := 0;v[3149] := 0;v[3150] := 0;v[3151] := 0;v[3152] := 0;v[3153] := 0;v[3154] := 0;v[3155] := 0;v[3156] := 0;v[3157] := 0;v[3158] := 0;v[3159] := 0;v[3160] := 0;v[3161] := 0;v[3162] := 0;v[3163] := 0;v[3164] := 0;v[3165] := 0;v[3166] := 0;v[3167] := 0;v[3168] := 0;v[3169] := 0;v[3170] := 0;v[3171] := 0;v[3172] := 0;v[3173] := 0;v[3174] := 0;v[3175] := 0;v[3176] := 0;v[3177] := 0;v[3178] := 0;v[3179] := 0;v[3180] := 0;v[3181] := 0;v[3182] := 0;v[3183] := 0;v[3184] := 0;v[3185] := 0;v[3186] := 0;v[3187] := 0;v[3188] := 0;v[3189] := 0;v[3190] := 0;v[3191] := 0;v[3192] := 0;v[3193] := 0;v[3194] := 0;v[3195] := 0;v[3196] := 0;v[3197] := 0;v[3198] := 0;v[3199] := 0;v[3200] := 0;v[3201] := 0;v[3202] := 0;v[3203] := 0;v[3204] := 0;v[3205] := 0;v[3206] := 0;v[3207] := 0;v[3208] := 0;v[3209] := 0;v[3210] := 0;v[3211] := 0;v[3212] := 0;v[3213] := 0;v[3214] := 0;v[3215] := 0;v[3216] := 0;v[3217] := 0;v[3218] := 0;v[3219] := 0;v[3220] := 0;v[3221] := 0;v[3222] := 0;v[3223] := 0;v[3224] := 0;v[3225] := 0;v[3226] := 0;v[3227] := 0;v[3228] := 0;v[3229] := 0;v[3230] := 0;v[3231] := 0;v[3232] := 0;v[3233] := 0;v[3234] := 0;v[3235] := 0;v[3236] := 0;v[3237] := 0;v[3238] := 0;v[3239] := 0;v[3240] := 0;v[3241] := 0;v[3242] := 0;v[3243] := 0;v[3244] := 0;v[3245] := 0;v[3246] := 0;v[3247] := 0;v[3248] := 0;v[3249] := 0;v[3250] := 0;v[3251] := 0;v[3252] := 0;v[3253] := 0;v[3254] := 0;v[3255] := 0;v[3256] := 0;v[3257] := 0;v[3258] := 0;v[3259] := 0;v[3260] := 0;v[3261] := 0;v[3262] := 0;v[3263] := 0;v[3264] := 0;v[3265] := 0;v[3266] := 0;v[3267] := 0;v[3268] := 0;v[3269] := 0;v[3270] := 0;v[3271] := 0;v[3272] := 0;v[3273] := 0;v[3274] := 0;v[3275] := 0;v[3276] := 0;v[3277] := 0;v[3278] := 0;v[3279] := 0;v[3280] := 0;v[3281] := 0;v[3282] := 0;v[3283] := 0;v[3284] := 0;v[3285] := 0;v[3286] := 0;v[3287] := 0;v[3288] := 0;v[3289] := 0;v[3290] := 0;v[3291] := 0;v[3292] := 0;v[3293] := 0;v[3294] := 0;v[3295] := 0;v[3296] := 0;v[3297] := 0;v[3298] := 0;v[3299] := 0;v[3300] := 0;v[3301] := 0;v[3302] := 0;v[3303] := 0;v[3304] := 0;v[3305] := 0;v[3306] := 0;v[3307] := 0;v[3308] := 0;v[3309] := 0;v[3310] := 0;v[3311] := 0;v[3312] := 0;v[3313] := 0;v[3314] := 0;v[3315] := 0;v[3316] := 0;v[3317] := 0;v[3318] := 0;v[3319] := 0;v[3320] := 0;v[3321] := 0;v[3322] := 0;v[3323] := 0;v[3324] := 0;v[3325] := 0;v[3326] := 0;v[3327] := 0;v[3328] := 0;v[3329] := 0;v[3330] := 0;v[3331] := 0;v[3332] := 0;v[3333] := 0;v[3334] := 0;v[3335] := 0;v[3336] := 0;v[3337] := 0;v[3338] := 0;v[3339] := 0;v[3340] := 0;v[3341] := 0;v[3342] := 0;v[3343] := 0;v[3344] := 0;v[3345] := 0;v[3346] := 0;v[3347] := 0;v[3348] := 0;v[3349] := 0;v[3350] := 0;v[3351] := 0;v[3352] := 0;v[3353] := 0;v[3354] := 0;v[3355] := 0;v[3356] := 0;v[3357] := 0;v[3358] := 0;v[3359] := 0;v[3360] := 0;v[3361] := 0;v[3362] := 0;v[3363] := 0;v[3364] := 0;v[3365] := 0;v[3366] := 0;v[3367] := 0;v[3368] := 0;v[3369] := 0;v[3370] := 0;v[3371] := 0;v[3372] := 0;v[3373] := 0;v[3374] := 0;v[3375] := 0;v[3376] := 0;v[3377] := 0;v[3378] := 0;v[3379] := 0;v[3380] := 0;v[3381] := 0;v[3382] := 0;v[3383] := 0;v[3384] := 0;v[3385] := 0;v[3386] := 0;v[3387] := 0;v[3388] := 0;v[3389] := 0;v[3390] := 0;v[3391] := 0;v[3392] := 0;v[3393] := 0;v[3394] := 0;v[3395] := 0;v[3396] := 0;v[3397] := 0;v[3398] := 0;v[3399] := 0;v[3400] := 0;v[3401] := 0;v[3402] := 0;v[3403] := 0;v[3404] := 0;v[3405] := 0;v[3406] := 0;v[3407] := 0;v[3408] := 0;v[3409] := 0;v[3410] := 0;v[3411] := 0;v[3412] := 0;v[3413] := 0;v[3414] := 0;v[3415] := 0;v[3416] := 0;v[3417] := 0;v[3418] := 0;v[3419] := 0;v[3420] := 0;v[3421] := 0;v[3422] := 0;v[3423] := 0;v[3424] := 0;v[3425] := 0;v[3426] := 0;v[3427] := 0;v[3428] := 0;v[3429] := 0;v[3430] := 0;v[3431] := 0;v[3432] := 0;v[3433] := 0;v[3434] := 0;v[3435] := 0;v[3436] := 0;v[3437] := 0;v[3438] := 0;v[3439] := 0;v[3440] := 0;v[3441] := 0;v[3442] := 0;v[3443] := 0;v[3444] := 0;v[3445] := 0;v[3446] := 0;v[3447] := 0;v[3448] := 0;v[3449] := 0;v[3450] := 0;v[3451] := 0;v[3452] := 0;v[3453] := 0;v[3454] := 0;v[3455] := 0;v[3456] := 0;v[3457] := 0;v[3458] := 0;v[3459] := 0;v[3460] := 0;v[3461] := 0;v[3462] := 0;v[3463] := 0;v[3464] := 0;v[3465] := 0;v[3466] := 0;v[3467] := 0;v[3468] := 0;v[3469] := 0;v[3470] := 0;v[3471] := 0;v[3472] := 0;v[3473] := 0;v[3474] := 0;v[3475] := 0;v[3476] := 0;v[3477] := 0;v[3478] := 0;v[3479] := 0;v[3480] := 0;v[3481] := 0;v[3482] := 0;v[3483] := 0;v[3484] := 0;v[3485] := 0;v[3486] := 0;v[3487] := 0;v[3488] := 0;v[3489] := 0;v[3490] := 0;v[3491] := 0;v[3492] := 0;v[3493] := 0;v[3494] := 0;v[3495] := 0;v[3496] := 0;v[3497] := 0;v[3498] := 0;v[3499] := 0;v[3500] := 0;v[3501] := 0;v[3502] := 0;v[3503] := 0;v[3504] := 0;v[3505] := 0;v[3506] := 0;v[3507] := 0;v[3508] := 0;v[3509] := 0;v[3510] := 0;v[3511] := 0;v[3512] := 0;v[3513] := 0;v[3514] := 0;v[3515] := 0;v[3516] := 0;v[3517] := 0;v[3518] := 0;v[3519] := 0;v[3520] := 0;v[3521] := 0;v[3522] := 0;v[3523] := 0;v[3524] := 0;v[3525] := 0;v[3526] := 0;v[3527] := 0;v[3528] := 0;v[3529] := 0;v[3530] := 0;v[3531] := 0;v[3532] := 0;v[3533] := 0;v[3534] := 0;v[3535] := 0;v[3536] := 0;v[3537] := 0;v[3538] := 0;v[3539] := 0;v[3540] := 0;v[3541] := 0;v[3542] := 0;v[3543] := 0;v[3544] := 0;v[3545] := 0;v[3546] := 0;v[3547] := 0;v[3548] := 0;v[3549] := 0;v[3550] := 0;v[3551] := 0;v[3552] := 0;v[3553] := 0;v[3554] := 0;v[3555] := 0;v[3556] := 0;v[3557] := 0;v[3558] := 0;v[3559] := 0;v[3560] := 0;v[3561] := 0;v[3562] := 0;v[3563] := 0;v[3564] := 0;v[3565] := 0;v[3566] := 0;v[3567] := 0;v[3568] := 0;v[3569] := 0;v[3570] := 0;v[3571] := 0;v[3572] := 0;v[3573] := 0;v[3574] := 0;v[3575] := 0;v[3576] := 0;v[3577] := 0;v[3578] := 0;v[3579] := 0;v[3580] := 0;v[3581] := 0;v[3582] := 0;v[3583] := 0;v[3584] := 0;v[3585] := 0;v[3586] := 0;v[3587] := 0;v[3588] := 0;v[3589] := 0;v[3590] := 0;v[3591] := 0;v[3592] := 0;v[3593] := 0;v[3594] := 0;v[3595] := 0;v[3596] := 0;v[3597] := 0;v[3598] := 0;v[3599] := 0;v[3600] := 0;v[3601] := 0;v[3602] := 0;v[3603] := 0;v[3604] := 0;v[3605] := 0;v[3606] := 0;v[3607] := 0;v[3608] := 0;v[3609] := 0;v[3610] := 0;v[3611] := 0;v[3612] := 0;v[3613] := 0;v[3614] := 0;v[3615] := 0;v[3616] := 0;v[3617] := 0;v[3618] := 0;v[3619] := 0;v[3620] := 0;v[3621] := 0;v[3622] := 0;v[3623] := 0;v[3624] := 0;v[3625] := 0;v[3626] := 0;v[3627] := 0;v[3628] := 0;v[3629] := 0;v[3630] := 0;v[3631] := 0;v[3632] := 0;v[3633] := 0;v[3634] := 0;v[3635] := 0;v[3636] := 0;v[3637] := 0;v[3638] := 0;v[3639] := 0;v[3640] := 0;v[3641] := 0;v[3642] := 0;v[3643] := 0;v[3644] := 0;v[3645] := 0;v[3646] := 0;v[3647] := 0;v[3648] := 0;v[3649] := 0;v[3650] := 0;v[3651] := 0;v[3652] := 0;v[3653] := 0;v[3654] := 0;v[3655] := 0;v[3656] := 0;v[3657] := 0;v[3658] := 0;v[3659] := 0;v[3660] := 0;v[3661] := 0;v[3662] := 0;v[3663] := 0;v[3664] := 0;v[3665] := 0;v[3666] := 0;v[3667] := 0;v[3668] := 0;v[3669] := 0;v[3670] := 0;v[3671] := 0;v[3672] := 0;v[3673] := 0;v[3674] := 0;v[3675] := 0;v[3676] := 0;v[3677] := 0;v[3678] := 0;v[3679] := 0;v[3680] := 0;v[3681] := 0;v[3682] := 0;v[3683] := 0;v[3684] := 0;v[3685] := 0;v[3686] := 0;v[3687] := 0;v[3688] := 0;v[3689] := 0;v[3690] := 0;v[3691] := 0;v[3692] := 0;v[3693] := 0;v[3694] := 0;v[3695] := 0;v[3696] := 0;v[3697] := 0;v[3698] := 0;v[3699] := 0;v[3700] := 0;v[3701] := 0;v[3702] := 0;v[3703] := 0;v[3704] := 0;v[3705] := 0;v[3706] := 0;v[3707] := 0;v[3708] := 0;v[3709] := 0;v[3710] := 0;v[3711] := 0;v[3712] := 0;v[3713] := 0;v[3714] := 0;v[3715] := 0;v[3716] := 0;v[3717] := 0;v[3718] := 0;v[3719] := 0;v[3720] := 0;v[3721] := 0;v[3722] := 0;v[3723] := 0;v[3724] := 0;v[3725] := 0;v[3726] := 0;v[3727] := 0;v[3728] := 0;v[3729] := 0;v[3730] := 0;v[3731] := 0;v[3732] := 0;v[3733] := 0;v[3734] := 0;v[3735] := 0;v[3736] := 0;v[3737] := 0;v[3738] := 0;v[3739] := 0;v[3740] := 0;v[3741] := 0;v[3742] := 0;v[3743] := 0;v[3744] := 0;v[3745] := 0;v[3746] := 0;v[3747] := 0;v[3748] := 0;v[3749] := 0;v[3750] := 0;v[3751] := 0;v[3752] := 0;v[3753] := 0;v[3754] := 0;v[3755] := 0;v[3756] := 0;v[3757] := 0;v[3758] := 0;v[3759] := 0;v[3760] := 0;v[3761] := 0;v[3762] := 0;v[3763] := 0;v[3764] := 0;v[3765] := 0;v[3766] := 0;v[3767] := 0;v[3768] := 0;v[3769] := 0;v[3770] := 0;v[3771] := 0;v[3772] := 0;v[3773] := 0;v[3774] := 0;v[3775] := 0;v[3776] := 0;v[3777] := 0;v[3778] := 0;v[3779] := 0;v[3780] := 0;v[3781] := 0;v[3782] := 0;v[3783] := 0;v[3784] := 0;v[3785] := 0;v[3786] := 0;v[3787] := 0;v[3788] := 0;v[3789] := 0;v[3790] := 0;v[3791] := 0;v[3792] := 0;v[3793] := 0;v[3794] := 0;v[3795] := 0;v[3796] := 0;v[3797] := 0;v[3798] := 0;v[3799] := 0;v[3800] := 0;v[3801] := 0;v[3802] := 0;v[3803] := 0;v[3804] := 0;v[3805] := 0;v[3806] := 0;v[3807] := 0;v[3808] := 0;v[3809] := 0;v[3810] := 0;v[3811] := 0;v[3812] := 0;v[3813] := 0;v[3814] := 0;v[3815] := 0;v[3816] := 0;v[3817] := 0;v[3818] := 0;v[3819] := 0;v[3820] := 0;v[3821] := 0;v[3822] := 0;v[3823] := 0;v[3824] := 0;v[3825] := 0;v[3826] := 0;v[3827] := 0;v[3828] := 0;v[3829] := 0;v[3830] := 0;v[3831] := 0;v[3832] := 0;v[3833] := 0;v[3834] := 0;v[3835] := 0;v[3836] := 0;v[3837] := 0;v[3838] := 0;v[3839] := 0;v[3840] := 0;v[3841] := 0;v[3842] := 0;v[3843] := 0;v[3844] := 0;v[3845] := 0;v[3846] := 0;v[3847] := 0;v[3848] := 0;v[3849] := 0;v[3850] := 0;v[3851] := 0;v[3852] := 0;v[3853] := 0;v[3854] := 0;v[3855] := 0;v[3856] := 0;v[3857] := 0;v[3858] := 0;v[3859] := 0;v[3860] := 0;v[3861] := 0;v[3862] := 0;v[3863] := 0;v[3864] := 0;v[3865] := 0;v[3866] := 0;v[3867] := 0;v[3868] := 0;v[3869] := 0;v[3870] := 0;v[3871] := 0;v[3872] := 0;v[3873] := 0;v[3874] := 0;v[3875] := 0;v[3876] := 0;v[3877] := 0;v[3878] := 0;v[3879] := 0;v[3880] := 0;v[3881] := 0;v[3882] := 0;v[3883] := 0;v[3884] := 0;v[3885] := 0;v[3886] := 0;v[3887] := 0;v[3888] := 0;v[3889] := 0;v[3890] := 0;v[3891] := 0;v[3892] := 0;v[3893] := 0;v[3894] := 0;v[3895] := 0;v[3896] := 0;v[3897] := 0;v[3898] := 0;v[3899] := 0;v[3900] := 0;v[3901] := 0;v[3902] := 0;v[3903] := 0;v[3904] := 0;v[3905] := 0;v[3906] := 0;v[3907] := 0;v[3908] := 0;v[3909] := 0;v[3910] := 0;v[3911] := 0;v[3912] := 0;v[3913] := 0;v[3914] := 0;v[3915] := 0;v[3916] := 0;v[3917] := 0;v[3918] := 0;v[3919] := 0;v[3920] := 0;v[3921] := 0;v[3922] := 0;v[3923] := 0;v[3924] := 0;v[3925] := 0;v[3926] := 0;v[3927] := 0;v[3928] := 0;v[3929] := 0;v[3930] := 0;v[3931] := 0;v[3932] := 0;v[3933] := 0;v[3934] := 0;v[3935] := 0;v[3936] := 0;v[3937] := 0;v[3938] := 0;v[3939] := 0;v[3940] := 0;v[3941] := 0;v[3942] := 0;v[3943] := 0;v[3944] := 0;v[3945] := 0;v[3946] := 0;v[3947] := 0;v[3948] := 0;v[3949] := 0;v[3950] := 0;v[3951] := 0;v[3952] := 0;v[3953] := 0;v[3954] := 0;v[3955] := 0;v[3956] := 0;v[3957] := 0;v[3958] := 0;v[3959] := 0;v[3960] := 0;v[3961] := 0;v[3962] := 0;v[3963] := 0;v[3964] := 0;v[3965] := 0;v[3966] := 0;v[3967] := 0;v[3968] := 0;v[3969] := 0;v[3970] := 0;v[3971] := 0;v[3972] := 0;v[3973] := 0;v[3974] := 0;v[3975] := 0;v[3976] := 0;v[3977] := 0;v[3978] := 0;v[3979] := 0;v[3980] := 0;v[3981] := 0;v[3982] := 0;v[3983] := 0;v[3984] := 0;v[3985] := 0;v[3986] := 0;v[3987] := 0;v[3988] := 0;v[3989] := 0;v[3990] := 0;v[3991] := 0;v[3992] := 0;v[3993] := 0;v[3994] := 0;v[3995] := 0;v[3996] := 0;v[3997] := 0;v[3998] := 0;v[3999] := 0;v[4000] := 0;v[4001] := 0;v[4002] := 0;v[4003] := 0;v[4004] := 0;v[4005] := 0;v[4006] := 0;v[4007] := 0;v[4008] := 0;v[4009] := 0;v[4010] := 0;v[4011] := 0;v[4012] := 0;v[4013] := 0;v[4014] := 0;v[4015] := 0;v[4016] := 0;v[4017] := 0;v[4018] := 0;v[4019] := 0;v[4020] := 0;v[4021] := 0;v[4022] := 0;v[4023] := 0;v[4024] := 0;v[4025] := 0;v[4026] := 0;v[4027] := 0;v[4028] := 0;v[4029] := 0;v[4030] := 0;v[4031] := 0;v[4032] := 0;v[4033] := 0;v[4034] := 0;v[4035] := 0;v[4036] := 0;v[4037] := 0;v[4038] := 0;v[4039] := 0;v[4040] := 0;v[4041] := 0;v[4042] := 0;v[4043] := 0;v[4044] := 0;v[4045] := 0;v[4046] := 0;v[4047] := 0;v[4048] := 0;v[4049] := 0;v[4050] := 0;v[4051] := 0;v[4052] := 0;v[4053] := 0;v[4054] := 0;v[4055] := 0;v[4056] := 0;v[4057] := 0;v[4058] := 0;v[4059] := 0;v[4060] := 0;v[4061] := 0;v[4062] := 0;v[4063] := 0;v[4064] := 0;v[4065] := 0;v[4066] := 0;v[4067] := 0;v[4068] := 0;v[4069] := 0;v[4070] := 0;v[4071] := 0;v[4072] := 0;v[4073] := 0;v[4074] := 0;v[4075] := 0;v[4076] := 0;v[4077] := 0;v[4078] := 0;v[4079] := 0;v[4080] := 0;v[4081] := 0;v[4082] := 0;v[4083] := 0;v[4084] := 0;v[4085] := 0;v[4086] := 0;v[4087] := 0;v[4088] := 0;v[4089] := 0;v[4090] := 0;v[4091] := 0;v[4092] := 0;v[4093] := 0;v[4094] := 0;v[4095] := 0;
    *[
    cmd_chan?cmd;
    [ cmd & 1 = 0 ->
    [
    ([]: j:4096:  (cmd >> 1) = j   ->  tmp := v[j]  )
    ];
    read_chan!tmp
    [] cmd & 1 = 1 ->
    skip
    ; write_chan?tmp;
    [
    ([]: j:%{Array.length init#Int}:  (cmd >> 1) = j   ->  v[j] := tmp  )
    ]
    ]
    ]
    }
    }


    defproc main(chan?(int<8>) user_i0;chan!(int<8>) user_o0) {

      chan(int<8>) c0;
      chan(int<8>) c1;
      chan(int<8>) c2;
      chan(int<13>) c3;
      chan(int<8>) c4;
      chan(int<13>) c5;
      chan(int<8>) c6;
    proc_0 proc_0_ (c0,c1,c2,c3,c4,c5,c6);
    proc_1 proc_1_ (c3,c0);
    proc_2 proc_2_ (c5,c1,c4);
    c2 = user_i0;
    c6 = user_o0;
    } |}]
