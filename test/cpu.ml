open! Core
open! Act

module Instr = struct
  (* TODO autogenerate this with a ppx *)
  module T = struct
    type t =
      | End
      | Nop
      | Push_imm
        (* push the next instruction, and then increase the program counter by 2 *)
      | Dup (* pop a; push a; push a *)
      | Exch (* pop a; pop b; push a; push b *)
      | Exch2
        (* pop a; pop b; pop c; push a; push c; push b;   [TOP;a;b;c] -> [TOP;b;c;a] *)
      | Jump (* pop addr_high; pop addr_loc; goto addr *)
      | JumpIfNot
        (*  pop addr_high; pop addr_loc; pop flag; if (flag == 0) then goto addr *)
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
  include Enum.Make (T)
end

let cpu instrs ~ochan ~ichan =
  let tmp0 = Var.create CInt.dtype_8 in
  let tmp1 = Var.create CInt.dtype_8 in
  let tmp2 = Var.create CInt.dtype_8 in

  let addr_high = Var.create CInt.dtype_8 in
  let addr_low = Var.create CInt.dtype_8 in
  let flag = Var.create CInt.dtype_8 in

  (* program code *)
  let instrs = Mem.create_ug_rom CInt.dtype_8 instrs in
  let pc = Var.create (CInt.dtype ~bits:12) ~init:(CInt.of_int 0) in

  (* stack *)
  let stack = Array.init 4096 ~f:(fun _ -> CInt.zero) in
  let stack = Mem.create_ug_mem CInt.dtype_8 stack in
  let sp = Var.create (CInt.dtype ~bits:12) ~init:(CInt.of_int 0) in

  (* ir *)
  let done_ = Var.create CBool.dtype ~init:false in
  let push value =
    N.seq
      [
        (* N.log1' value ~f:(fun value -> sprintf "push %d\n" (CInt.to_int_exn value)); *)
        N.write_ug_mem stack ~idx:CInt.E.(var sp) ~value;
        CInt.N.incr sp ~overflow:Cant;
      ]
  in
  let pop ~dst =
    N.seq
      [
        CInt.N.decr sp ~underflow:Cant;
        N.read_ug_mem stack ~idx:CInt.E.(var sp) ~dst;
        (* N.log1 dst ~f:(fun dst -> sprintf "pop %d\n" (CInt.to_int_exn dst)); *)
      ]
  in
  let set_pc_to_addr ~addr_high ~addr_low =
    let expr =
      CInt.E.(
        lshift (var addr_high) ~amt:(cint 8)
        |> bit_or (var addr_low)
        |> clip ~bits:12)
    in
    N.assign pc expr
  in

  N.while_loop
    CBool.E.(not_ (var done_))
    [
      N.read_ug_rom instrs ~idx:Expr.(var pc) ~dst:tmp0;
      (* N.log "\n"; *)
      (* N.log1 pc ~f:(fun pc -> sprintf "pc = %d\n" (CInt.to_int_exn pc)); *)
      (* N.log1 tmp0 ~f:(fun instr -> sprintf "instr = %d\n" (CInt.to_int_exn instr)); *)
      (* N.log1 sp ~f:(fun sp -> sprintf "sp = %d\n" (CInt.to_int_exn sp)); *)
      Instr.N.match_
        (Expr.var tmp0 |> Instr.E.of_int)
        ~f:(function
          | End -> N.assign done_ CBool.E.true_
          | Nop -> CInt.N.incr pc ~overflow:Cant
          | Push_imm ->
              (* push the next instruction, and then increase the program counter by 2 *)
              N.seq
                [
                  CInt.N.incr pc ~overflow:Cant;
                  N.read_ug_rom instrs ~idx:(Expr.var pc) ~dst:tmp0;
                  push Expr.(var tmp0);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Dup ->
              N.seq
                [
                  pop ~dst:tmp0;
                  push (Expr.var tmp0);
                  push (Expr.var tmp0);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Exch ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  push (Expr.var tmp0);
                  push (Expr.var tmp1);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Exch2 ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  pop ~dst:tmp2;
                  push (Expr.var tmp0);
                  push (Expr.var tmp2);
                  push (Expr.var tmp1);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Jump ->
              N.seq
                [
                  pop ~dst:addr_high;
                  pop ~dst:addr_low;
                  set_pc_to_addr ~addr_high ~addr_low;
                ]
          | JumpIfNot ->
              (* pop flag; pop addr_high; pop addr_loc; if (flag == 0) then goto addr *)
              N.seq
                [
                  pop ~dst:addr_high;
                  pop ~dst:addr_low;
                  pop ~dst:flag;
                  CBool.N.match_
                    CInt.E.(eq (var flag) (cint 0))
                    ~f:(function
                      | true -> set_pc_to_addr ~addr_high ~addr_low
                      | false -> CInt.N.incr pc ~overflow:Cant);
                ]
          | Eq ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  push (CInt.E.(eq (var tmp0) (var tmp1)) |> CBool.E.to_int);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Add ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  push CInt.E.(add_wrap (var tmp0) (var tmp1) ~bits:8);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Sub ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  push CInt.E.(sub_wrap (var tmp1) (var tmp0) ~bits:8);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Bool_or ->
              N.seq
                [
                  pop ~dst:tmp0;
                  pop ~dst:tmp1;
                  push
                    CInt.E.(
                      bit_or
                        (var tmp0 |> ne zero |> CBool.E.to_int)
                        (var tmp1 |> ne zero |> CBool.E.to_int));
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Bool_not ->
              N.seq
                [
                  pop ~dst:tmp0;
                  push CInt.E.(var tmp0 |> eq zero |> CBool.E.to_int);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Input ->
              N.seq
                [
                  N.read ichan tmp0;
                  push Expr.(var tmp0);
                  CInt.N.incr pc ~overflow:Cant;
                ]
          | Output ->
              N.seq
                [
                  pop ~dst:tmp0;
                  N.send ochan (Expr.var tmp0);
                  CInt.N.incr pc ~overflow:Cant;
                ]);
    ]

let test instrs =
  let my_instrs =
    Array.init 4096 ~f:(fun i ->
        if i < Array.length instrs then instrs.(i) else Instr.to_int Instr.Nop)
  in
  let ichan = Chan.create CInt.dtype_8 in
  let ochan = Chan.create CInt.dtype_8 in
  let ir = cpu my_instrs ~ichan:ichan.r ~ochan:ochan.w in
  let ichan, ochan = (ichan.w, ochan.r) in
  let sim =
    Sim.create ir ~user_sendable_ports:[ ichan.u ]
      ~user_readable_ports:[ ochan.u ]
  in
  (sim, ichan, ochan)

let%expect_test "test" =
  let instrs =
    [|
      Instr.to_int Push_imm;
      CInt.of_int 3;
      Instr.to_int Output;
      Instr.to_int End;
    |]
  in
  let sim, _, o = test instrs in
  Sim.read sim o (CInt.of_int 3);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {|
      (Ok ()) |}]

let%expect_test "test" =
  let instrs =
    [|
      Instr.to_int Input;
      Instr.to_int Output;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Jump;
    |]
  in
  let sim, i, o = test instrs in
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
      Instr.to_int Push_imm;
      CInt.of_int 54;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      (* [TOP; return_addr_high; return_addr_low]; *)
      Instr.to_int Input;
      (* [TOP; n; return_addr_high; return_addr_low]; *)
      (* just slide into the function code! *)

      (* BEGIN FUNCTION - fib *)
      (* Input: [TOP; n; return_addr_high; return_addr_low];
         Output: [TOP; fib(n)] and pc = return_addr *)
      Instr.to_int Dup;
      Instr.to_int Dup;
      (* [TOP; n; n; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Push_imm;
      CInt.of_int 1;
      Instr.to_int Eq;
      (* [TOP; n == 1; n; n; return_addr_high; return_addr_low]; *)
      (* 10: *)
      Instr.to_int Exch;
      (* [TOP; n; n == 1; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Eq;
      (* [TOP; n == 0; n == 1; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Bool_or;
      Instr.to_int Bool_not;
      (* [TOP; !(n == 1 || n == 0); n; return_addr_high; return_addr_low]; *)

      (* skip all the rest of the code if n = 1 *)
      Instr.to_int Push_imm;
      CInt.of_int 52;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      (* 20: *) Instr.to_int JumpIfNot;
      (* [TOP; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Dup;
      Instr.to_int Push_imm;
      CInt.of_int 1;
      Instr.to_int Sub;
      (* [TOP; n-1; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Push_imm;
      CInt.of_int 36;
      Instr.to_int Exch;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      (* 30: *) Instr.to_int Exch;
      (* [TOP; n-1; rec1_return_addr_high; rec1_return_addr_low; n; return_addr_high; return_addr_low]; *)
      (* push the start of the function *)
      Instr.to_int Push_imm;
      CInt.of_int 5;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      (* [TOP; func_start_high; func_start_low; n-1; rec1_return_addr_high; rec1_return_addr_low; n; return_addr_high; return_addr_low]; *)
      Instr.to_int Jump;
      (* [TOP; fib(n-1); n; return_addr_high; return_addr_low]; *)

      (* [TOP; fib(n-1); n; return_addr_high; return_addr_low]; *)
      Instr.to_int Exch;
      Instr.to_int Push_imm;
      CInt.of_int 2;
      Instr.to_int Sub;
      (* [TOP; n-2; fib(n-1);  return_addr_high; return_addr_low]; *)
      (* 40: *)
      Instr.to_int Push_imm;
      CInt.of_int 51;
      Instr.to_int Exch;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Exch;
      (* [TOP; n-2; rec1_return_addr_high; rec1_return_addr_low; fib(n-1); return_addr_high; return_addr_low]; *)
      (* push the start of the function *)
      Instr.to_int Push_imm;
      CInt.of_int 5;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      (* [TOP; func_start_high; func_start_low; n-2; rec1_return_addr_high; rec1_return_addr_low; fib(n-1); return_addr_high; return_addr_low]; *)
      (* 50: *)
      Instr.to_int Jump;
      (* [TOP; fib(n-2); fib(n-1); return_addr_high; return_addr_low]; *)
      Instr.to_int Add;
      (* [TOP; fib(n); return_addr_high; return_addr_low]; *)

      (* so now in either case it has fib(n) on the top of the stack. So reorder the stack and return. *)
      Instr.to_int Exch2;
      (* [TOP; return_addr_high; return_addr_low]; fib(n); *)
      Instr.to_int Jump;
      (* final outputing code *)
      Instr.to_int Output;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Push_imm;
      CInt.of_int 0;
      Instr.to_int Jump;
    |]
  in
  let sim, i, o = test instrs in
  Sim.send sim i (CInt.of_int 0);
  Sim.read sim o (CInt.of_int 0);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 1);
  Sim.read sim o (CInt.of_int 1);
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 2);
  Sim.read sim o (CInt.of_int 1);
  print_s [%sexp (Sim.wait sim ~max_steps:10000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 7);
  Sim.read sim o (CInt.of_int 13);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 8);
  Sim.read sim o (CInt.of_int 21);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  Sim.send sim i (CInt.of_int 12);
  Sim.read sim o (CInt.of_int 144);
  print_s [%sexp (Sim.wait sim ~max_steps:1000000 () : unit Or_error.t)];
  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ()) |}]
