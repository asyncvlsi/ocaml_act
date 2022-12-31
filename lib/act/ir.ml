


open! Core

module Var_id : sig
  type t

  include Identifiable with type t := t

  val of_int : int -> t
  val to_assem_id : t -> Assem.Var_id.t
end = struct
  include Int

  let to_assem_id t = Assem.Var_id.of_int t
end

module Chan_id : sig
  type t

  include Identifiable with type t := t

  val of_int : int -> t
end = struct
  include Int
end

module Expr = struct
  type t = Var of Var_id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
  [@@deriving sexp]

  let _sexp_of_t _ = String.sexp_of_t "<expr>"
  let const c = Const (Obj.magic c)
  let map e ~f = Map (e, Obj.magic f)

  (*
     let rec var_ids t =
       match t with Var id -> [ id ] | Const _ -> [] | Map (e, _) -> var_ids e
  *)
  let rec to_assem_expr = function
    | Var var -> Assem.Expr.Var (Var_id.to_assem_id var)
    | Const c -> Const c
    | Map (v, f) -> Map (to_assem_expr v, f)
end

module N = struct
  type t =
    | Assign of Var_id.t * Expr.t
    | Log of Expr.t
    | Assert of Expr.t
    | Seq of t list
    | Par of t list
    | IfElse of Expr.t * (* true *) t * (* false *) t
    | Read of (* chan *) Chan_id.t * (* var *) Var_id.t
    | Send of (* chan *) Chan_id.t * (* var *) Expr.t
    | Loop of t
    | WhileLoop of Expr.t * t
end

module Sim = struct
  type t = {
    sim : Assem.Sim.t;
    user_readable_tbl :
      (Assem.Var_id.t * Assem.Instr_idx.t * Assem.Chan_buff.t) Chan_id.Table.t;
    user_sendable_tbl :
      (Assem.Var_id.t * Assem.Instr_idx.t * Assem.Chan_buff.t) Chan_id.Table.t;
  }

  let assem_of_ir t =
    (* TODO use a vector *)
    let module AB = Assem.Builder in
    let instrs = AB.create () in
    (* let push assem =
         instrs.(!instr_idx) <- assem;
         incr instr_idx;
         !instr_idx - 1
       in
       let edit idx assem = instrs.(idx) <- assem in
       let next_idx () = !instr_idx in *)
    let chan_tbl = Chan_id.Table.create () in
    let get_chan chan_id =
      Hashtbl.find_or_add chan_tbl chan_id ~default:(fun () ->
          Assem.Chan_buff.create ())
    in

    let rec convert stmt =
      let convert' stmt = convert stmt |> fun _ -> () in
      match stmt with
      | N.Assign (id, expr) ->
          AB.push instrs
            (Assem.N.Assign (Var_id.to_assem_id id, Expr.to_assem_expr expr))
      | Log expr -> AB.push instrs (Log (Expr.to_assem_expr expr))
      | Assert expr -> AB.push instrs (Assert (Expr.to_assem_expr expr))
      | Seq stmts -> List.map stmts ~f:convert |> List.last_exn
      | Par stmts ->
          let split = AB.push instrs (Par []) in
          let ends =
            List.map stmts ~f:(fun stmt ->
                convert' stmt;
                AB.push instrs (Jump Assem.Instr_idx.dummy_val))
          in
          let starts =
            List.take (split :: ends) (List.length stmts)
            |> List.map ~f:Assem.Instr_idx.next
          in
          let merge =
            AB.push instrs
              (ParJoin (Assem.Par_join.create ~max_ct:(List.length stmts)))
          in
          AB.edit instrs split (Par starts);
          List.iter ends ~f:(fun end_ -> AB.edit instrs end_ (Jump merge));
          merge
      | IfElse (expr, t_branch, f_branch) ->
          let split =
            AB.push instrs
              (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.dummy_val))
          in
          convert' t_branch;
          let t_end = AB.push instrs (Jump Assem.Instr_idx.dummy_val) in
          let f_end = convert f_branch in
          AB.edit instrs split
            (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.next t_end));
          AB.edit instrs t_end (Jump (Assem.Instr_idx.next f_end));
          f_end
      | Read (chan, var) ->
          let chan = get_chan chan in
          let read = AB.push instrs (Read (Var_id.to_assem_id var, chan)) in
          read
      | Send (chan, expr) ->
          let chan = get_chan chan in
          let send = AB.push instrs (Send (Expr.to_assem_expr expr, chan)) in
          send
      | Loop seq ->
          let fst = AB.next_idx instrs in
          convert' seq;
          let jmp = AB.push instrs (Jump fst) in
          jmp
      | WhileLoop (expr, seq) ->
          let split =
            AB.push instrs
              (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.dummy_val))
          in
          convert' seq;
          let jmp = AB.push instrs (Jump split) in
          AB.edit instrs split
            (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.next jmp));
          jmp
    in
    let (_ : Assem.Instr_idx.t) = convert t in
    let (_ : Assem.Instr_idx.t) = AB.push instrs End in
    (* let instr = Array.sub instrs ~pos:0 ~len:(len + 1) in *)
    (instrs, chan_tbl)

  let create ir ~user_sendable_ports ~user_readable_ports =
    (* user_sendable_ports = inputs to the program, and user_readable_ports are outputs from the program *)
    assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
    let assem, chan_tbl = assem_of_ir ir in
    let var_ct =
      (Assem.Builder.to_array assem
      |> Array.to_list
      |> List.concat_map ~f:Assem.N.var_ids
      |> List.max_elt ~compare:Assem.Var_id.compare
      |> Option.map ~f:Assem.Var_id.to_int
      |> Option.value ~default:0)
      + 1
    in
    let var_ct = ref var_ct in
    let user_readable_tbl = Chan_id.Table.create () in
    Set.iter user_readable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem.Var_id.of_int !var_ct in
        incr var_ct;
        let inst_idx = Assem.Builder.push assem (Read (var_id, chan)) in
        let (_ : Assem.Instr_idx.t) = Assem.Builder.push assem ConsumePC in
        Hashtbl.set user_readable_tbl ~key:port ~data:(var_id, inst_idx, chan));
    let user_sendable_tbl = Chan_id.Table.create () in
    Set.iter user_sendable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem.Var_id.of_int !var_ct in
        incr var_ct;
        let inst_idx = Assem.Builder.push assem (Send (Var var_id, chan)) in
        let (_ : Assem.Instr_idx.t) = Assem.Builder.push assem ConsumePC in
        Hashtbl.set user_sendable_tbl ~key:port ~data:(var_id, inst_idx, chan));
    let sim = Assem.Sim.create (Assem.Builder.to_array assem) ~var_ct:!var_ct in
    { sim; user_sendable_tbl; user_readable_tbl }

  let wait ?(max_steps = 1000) t = Assem.Sim.wait ~max_steps t.sim

  let wait' ?(max_steps = 1000) t =
    let (_ : Assem.Sim.Wait_outcome.t) = Assem.Sim.wait ~max_steps t.sim in
    ()

  let send t chan_id value =
    let var_id, inst_idx, _chan =
      Hashtbl.find_exn t.user_sendable_tbl chan_id
    in
    Assem.Sim.Advanced.set_var t.sim ~var_id ~value;
    Assem.Sim.Advanced.add_pc t.sim inst_idx;
    (* TODO handle checking that send actually happens *)
    ()

  let read t chan_id _value =
    let _var_id, inst_idx, _chan =
      Hashtbl.find_exn t.user_readable_tbl chan_id
    in
    (* Var_table.set t.sim.var_table var_id val; *)
    Assem.Sim.Advanced.add_pc t.sim inst_idx;
    (* TODO handle checking that read actually happens, and that it matches the expected value *)
    ()
end

let%expect_test "test1" =
  let ir =
    let var0 = Var_id.of_int 0 in
    let var1 = Var_id.of_int 1 in
    let chan2 = Chan_id.of_int 2 in
    N.Seq
      [
        Assign (var0, Expr.const 12345);
        Par
          [
            Loop (Seq [ Send (chan2, Var var0) ]);
            Seq
              [
                Read (chan2, var1); Log (Var var1 |> Expr.map ~f:Int.to_string);
              ];
          ];
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:Chan_id.Set.empty
      ~user_readable_ports:Chan_id.Set.empty
  in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Assem.Sim.Wait_outcome.t)];
  [%expect {| 12345Stuck |}]

let%expect_test "test2" =
  let ir =
    let var0 = Var_id.of_int 0 in
    let var1 = Var_id.of_int 1 in
    N.Seq
      [
        Assign (var0, Expr.const 123456);
        Assign (var1, Expr.const 1);
        WhileLoop
          ( Expr.map (Var var0) ~f:(fun v -> v > 1),
            Seq
              [
                Assign (var1, Expr.map (Var var1) ~f:(fun v -> v + 1));
                IfElse
                  ( Expr.map (Var var0) ~f:(fun v -> v % 2 < 1),
                    Assign (var0, Expr.map (Var var0) ~f:(fun v -> v / 2)),
                    Assign (var0, Expr.map (Var var0) ~f:(fun v -> (v * 3) + 1))
                  );
              ] );
        Log (Var var1 |> Expr.map ~f:(fun v -> sprintf "%d\n" v));
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:Chan_id.Set.empty
      ~user_readable_ports:Chan_id.Set.empty
  in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Assem.Sim.Wait_outcome.t)];
  [%expect {|
    62
    Done |}]

let%expect_test "test3" =
  let var = Var_id.of_int 0 in
  let chan = Chan_id.of_int 1 in
  let ir =
    N.Seq
      [
        Log (Expr.const "start\n");
        Read (chan, var);
        Log (Expr.const "recv 1\n");
        Read (chan, var);
        Log (Expr.const "recv 2\n");
        Read (chan, var);
        Log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan_id.Set.of_list [ chan ])
      ~user_readable_ports:Chan_id.Set.empty
  in
  Sim.wait' sim;
  [%expect {| start |}];
  Sim.send sim chan (Obj.magic 100);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.send sim chan (Obj.magic 100);
  Sim.wait' sim;
  [%expect {| recv 2 |}];
  Sim.wait' sim;
  [%expect {| |}];
  Sim.send sim chan (Obj.magic 200);
  Sim.wait' sim;
  [%expect {| done |}]

let%expect_test "test3" =
  let var = Var_id.of_int 0 in
  let chan1 = Chan_id.of_int 1 in
  let chan2 = Chan_id.of_int 2 in
  let ir =
    N.Seq
      [
        Log (Expr.const "start\n");
        Read (chan1, var);
        Log (Expr.const "recv 1\n");
        Send (chan2, Var var);
        Log (Expr.const "send 1\n");
        Assert (Expr.map (Var var) ~f:(fun v -> Int.equal v 200));
        Log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan_id.Set.of_list [ chan1 ])
      ~user_readable_ports:(Chan_id.Set.of_list [ chan2 ])
  in
  Sim.wait' sim;
  [%expect {| start |}];
  Sim.send sim chan1 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.read sim chan2 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {|
    send 1
    done |}]

let%expect_test "test4" =
  let var = Var_id.of_int 0 in
  let chan1 = Chan_id.of_int 1 in
  let chan2 = Chan_id.of_int 2 in
  let ir =
    N.Seq
      [
        Log (Expr.const "start\n");
        Read (chan1, var);
        Log (Expr.const "recv 1\n");
        Send (chan2, Var var);
        Log (Expr.const "send 1\n");
        Assert (Expr.map (Var var) ~f:(fun v -> Int.equal v 210));
        Log (Expr.const "done\n");
      ]
  in
  let sim =
    Sim.create ir
      ~user_sendable_ports:(Chan_id.Set.of_list [ chan1 ])
      ~user_readable_ports:(Chan_id.Set.of_list [ chan2 ])
  in
  Sim.wait' sim;
  [%expect {| start |}];
  Sim.send sim chan1 (Obj.magic 200);
  Sim.wait' sim;
  [%expect {| recv 1 |}];
  Sim.read sim chan2 (Obj.magic 200);
  let wait_outcome = Sim.wait sim in
  print_s [%sexp (wait_outcome : Assem.Sim.Wait_outcome.t)];
  [%expect {|
    send 1
    (Assert_failure 5) |}]
