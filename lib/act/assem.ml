open! Core
module Var_id = Int

let queue_extend q o = Queue.iter o ~f:(fun e -> Queue.enqueue q e)

module Expr = struct
  type t = Var of Var_id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
  [@@deriving sexp]

  let sexp_of_t _ = String.sexp_of_t "<expr>"
  let const c = Const (Obj.magic c)
  let map e ~f = Map (e, Obj.magic f)

  let rec var_ids t =
    match t with Var id -> [ id ] | Const _ -> [] | Map (e, _) -> var_ids e
end

module Instr_idx = struct
  include Int

  let dummy_val = -1
  let next i = i + 1
end

module Chan_buff = struct
  type t = {
    mutable read_ready : bool;
    mutable read_instr : Instr_idx.t;
    mutable read_dst_var_id : Var_id.t;
    mutable send_ready : bool;
    mutable send_instr : Instr_idx.t;
    mutable send_expr : Expr.t;
  }
  [@@deriving sexp]

  let create () =
    {
      read_ready = false;
      send_ready = false;
      read_instr = Instr_idx.dummy_val;
      send_instr = Instr_idx.dummy_val;
      read_dst_var_id = 0;
      send_expr = Const (Obj.magic 0);
    }
end

module Par_join = struct
  type t = { max_ct : int; mutable curr_ct : int } [@@deriving sexp]

  let create ~max_ct = { max_ct; curr_ct = 0 }
end

module Send_enqueuer = struct
  type t = {
    to_send : Any.t Queue.t;
    var_id : Var_id.t;
    mutable is_done : bool;
  }
  [@@deriving sexp]

  let create ~var_id = { to_send = Queue.create (); var_id; is_done = true }
end

module Read_dequeuer = struct
  type t = {
    expected_reads : Any.t Queue.t;
    var_id : Var_id.t;
    equals : (Any.t -> Any.t -> bool[@sexp.opaque]);
  }
  [@@deriving sexp]

  let create ~var_id ~equals =
    { expected_reads = Queue.create (); var_id; equals }
end

module N = struct
  type t =
    | End
    | Unreachable
    | Nop
    | Assign of Var_id.t * Expr.t
    | Log of Expr.t
    | Assert of Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
    | SelectImm of (Expr.t * Instr_idx.t) list
    | SelectImmElse of (Expr.t * Instr_idx.t) list * Instr_idx.t
    | Read of Var_id.t * Chan_buff.t
    | Send of Expr.t * Chan_buff.t
    (* These are ``magic'' instructions that allow user io operations. These instruction
       should be placed immediatly after the assoiated send/read instruction *)
    | Send_enqueuer of Send_enqueuer.t
    | Read_dequeuer of Read_dequeuer.t
  [@@deriving sexp]

  let read_ids t =
    (match t with
    | End | Unreachable | Nop | Par _ | ParJoin _ -> []
    | Read (_, _) | Jump _ -> []
    | Assign (_, expr) -> Expr.var_ids expr
    | Log expr | Assert expr | JumpIfFalse (expr, _) -> Expr.var_ids expr
    | SelectImm l | SelectImmElse (l, _) ->
        List.concat_map l ~f:(fun (expr, _) -> Expr.var_ids expr)
    | Send (expr, _) -> Expr.var_ids expr
    | Send_enqueuer _ -> []
    | Read_dequeuer d -> [ d.var_id ])
    |> Var_id.Set.of_list

  let write_ids t =
    (match t with
    | End | Unreachable | Nop | Par _ | ParJoin _ -> []
    | Log _ | Assert _ | JumpIfFalse (_, _) | Jump _ -> []
    | SelectImm _ | SelectImmElse _ | Send (_, _) -> []
    | Assign (id, _) -> [ id ]
    | Read (var_id, _) -> [ var_id ]
    | Send_enqueuer d -> [ d.var_id ]
    | Read_dequeuer _ -> [])
    |> Var_id.Set.of_list

  let var_ids t = Set.union (read_ids t) (write_ids t) |> Set.to_list
end

module Assem_vec = struct
  type t = N.t Vec.t

  let create () = Vec.create ~cap:10 ~default:N.End

  let push t instr =
    Vec.push t instr;
    Vec.length t - 1

  let edit t idx assem = Vec.set t idx assem
  let next_idx t = Vec.length t
  let to_array t = Vec.to_array t
end

module Var_table = struct
  (* Note that the read count and a write count of a variable may simultaniously be >= 0 if
     there is an instruction that both reads and writes a variable. For example, `x := -x`. *)
  type t = {
    values : (Any.t array[@sexp.opaque]);
    read_cts : int array;
    write_cts : int array;
    is_inited : bool array;
  }
  [@@deriving sexp]

  let create ~ct =
    {
      values = Array.init ct ~f:(fun _ -> Obj.magic 0);
      read_cts = Array.init ct ~f:(fun _ -> 0);
      write_cts = Array.init ct ~f:(fun _ -> 0);
      is_inited = Array.init ct ~f:(fun _ -> false);
    }

  let ok_to_guard t ~read_ids ~write_ids ~pc =
    let to_unit_result o ~f =
      match o with Some v -> Error (f v) | None -> Ok ()
    in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id -> t.write_cts.(read_id) > 0)
      |> to_unit_result ~f:(fun read_id -> `Reading_written_var (read_id, pc))
    in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id -> not t.is_inited.(read_id))
      |> to_unit_result ~f:(fun read_id -> `Uninit_id (read_id, pc))
    in
    let%bind.Result () =
      Set.find write_ids ~f:(fun write_id -> t.read_cts.(write_id) > 0)
      |> to_unit_result ~f:(fun write_id -> `Writing_read_var (write_id, pc))
    in
    Set.find write_ids ~f:(fun write_id -> t.write_cts.(write_id) > 0)
    |> to_unit_result ~f:(fun write_id -> `Writing_written_var (write_id, pc))

  let rec eval t expr =
    match expr with
    | Expr.Var id -> t.values.(id)
    | Const c -> c
    | Map (e, f) -> f (eval t e)

  let eval_bool t expr : bool = eval t expr |> Obj.magic
  let eval_string t expr : string = eval t expr |> Obj.magic

  let guard t ~read_ids ~write_ids ~pc =
    let%map.Result () = ok_to_guard t ~read_ids ~write_ids ~pc in
    Set.iter read_ids ~f:(fun read_id ->
        t.read_cts.(read_id) <- t.read_cts.(read_id) + 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.write_cts.(write_id) <- t.write_cts.(write_id) + 1)

  let unguard t ~read_ids ~write_ids =
    Set.iter read_ids ~f:(fun read_id ->
        t.read_cts.(read_id) <- t.read_cts.(read_id) - 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.write_cts.(write_id) <- t.write_cts.(write_id) - 1)

  let guard' t assem pc =
    guard t
      ~read_ids:(N.read_ids assem.(pc))
      ~write_ids:(N.write_ids assem.(pc))
      ~pc

  let unguard' t assem pc =
    unguard t
      ~read_ids:(N.read_ids assem.(pc))
      ~write_ids:(N.write_ids assem.(pc))

  let at t ~var_id =
    assert t.is_inited.(var_id);
    t.values.(var_id)

  let set t ~var_id ~value =
    t.values.(var_id) <- value;
    t.is_inited.(var_id) <- true

  let assign t ~var_id ~expr = set t ~var_id ~value:(eval t expr)
end

let for_loop_else max_ct ~f ~else_ =
  let return = ref None in
  let ct = ref 0 in
  while !ct < max_ct && Option.is_none !return do
    (match f !ct with `Continue -> () | `Return d -> return := Some d);
    incr ct
  done;
  Option.value !return ~default:else_

module Sim = struct
  type t = {
    assem : N.t array;
    mutable pcs : Instr_idx.t Vec.t;
    mutable var_table : Var_table.t;
    rng : (Random.State.t[@sexp.opaque]);
    mutable is_done : bool;
    chan_buffs : Chan_buff.t list;
  }
  [@@deriving sexp]

  module Wait_outcome = struct
    type t =
      | Already_errored
      | Time_out
      | Stuck of
          (* how many steps the simulation ran for before getting stuck *) int
      | Assert_failure of Instr_idx.t
      | Uninit_id of Var_id.t * Instr_idx.t
      | Simul_chan_senders of Instr_idx.t * Instr_idx.t
      | Simul_chan_readers of Instr_idx.t * Instr_idx.t
      | Simul_read_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Simul_write_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Select_no_guards_true of Instr_idx.t
      | Select_multiple_guards_true of int list * Instr_idx.t
      | Read_dequeuer_wrong_value of Instr_idx.t * Any.t * Any.t
      | Read_dequeuer_not_done of Instr_idx.t
      | Send_enqueuer_not_done of Instr_idx.t
    [@@deriving sexp]
  end

  let create ?(seed = 0) assem ~var_ct =
    (match assem.(0) with
    | N.Jump _ -> ()
    | _ ->
        failwith
          "The first assembly instruction must be a jump. This ensures it \
           neither reads nor writes any values");
    let var_table = Var_table.create ~ct:var_ct in
    let pcs = Vec.of_array [| 0 |] ~default:(-1) in
    let rng = Random.State.make [| seed |] in
    let chan_buffs =
      Array.to_list assem
      |> List.filter_map ~f:(function
           | Send (_, chan) | Read (_, chan) -> Some chan
           | _ -> None)
    in
    { assem; pcs; var_table; rng; is_done = false; chan_buffs }

  let step' t ~pc_idx =
    let vec_incr vec n = Vec.set vec n (Vec.at vec n + 1) in
    let guard pc = Var_table.guard' t.var_table t.assem pc in
    let unguard pc = Var_table.unguard' t.var_table t.assem pc in
    let step_chan (chan : Chan_buff.t) =
      if chan.read_ready && chan.send_ready then (
        chan.read_ready <- false;
        chan.send_ready <- false;
        unguard chan.read_instr;
        unguard chan.send_instr;
        Var_table.assign t.var_table ~var_id:chan.read_dst_var_id
          ~expr:chan.send_expr;
        Vec.extend t.pcs [ chan.read_instr + 1; chan.send_instr + 1 ];
        let%bind.Result () = guard (chan.read_instr + 1) in
        guard (chan.send_instr + 1))
      else Ok ()
    in
    let pc = Vec.at t.pcs pc_idx in
    match t.assem.(pc) with
    | End ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        Ok ()
    | Unreachable -> failwith "reached an 'unreachable' instruction"
    | Nop ->
        (* unguard pc; *)
        vec_incr t.pcs pc_idx;
        guard (pc + 1)
    | Assign (var_id, expr) ->
        unguard pc;
        Var_table.assign t.var_table ~var_id ~expr;
        vec_incr t.pcs pc_idx;
        guard (pc + 1)
    | Assert expr ->
        unguard pc;
        if Var_table.eval_bool t.var_table expr then (
          vec_incr t.pcs pc_idx;
          guard (pc + 1))
        else Error (`Assert_failure pc)
    | Log expr ->
        unguard pc;
        printf "%s" (Var_table.eval_string t.var_table expr);
        vec_incr t.pcs pc_idx;
        guard (pc + 1)
    | Par instrs ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        List.map instrs ~f:(fun instr ->
            Vec.push t.pcs instr;
            guard instr)
        |> Result.all_unit
    | ParJoin d ->
        (* unguard pc; *)
        d.curr_ct <- d.curr_ct + 1;
        if Int.equal d.max_ct d.curr_ct then (
          d.curr_ct <- 0;
          vec_incr t.pcs pc_idx;
          guard (pc + 1))
        else (
          Vec.remove t.pcs pc_idx;
          Ok ())
    | Jump inst ->
        (* unguard pc; *)
        Vec.set t.pcs pc_idx inst;
        guard inst
    | JumpIfFalse (expr, inst) ->
        unguard pc;
        let new_pc =
          if Var_table.eval_bool t.var_table expr then pc + 1 else inst
        in
        Vec.set t.pcs pc_idx new_pc;
        guard new_pc
    | SelectImm l -> (
        unguard pc;
        match
          List.filter_mapi l ~f:(fun idx (expr, instr) ->
              if Var_table.eval_bool t.var_table expr then Some (instr, idx)
              else None)
        with
        | [] -> Error (`Select_no_guards_true pc)
        | [ (instr, _) ] ->
            Vec.set t.pcs pc_idx instr;
            guard instr
        | l -> Error (`Select_multiple_guards_true (pc, List.map l ~f:snd)))
    | SelectImmElse (l, else_) -> (
        unguard pc;
        match
          List.filter_mapi l ~f:(fun idx (expr, instr) ->
              if Var_table.eval_bool t.var_table expr then Some (instr, idx)
              else None)
        with
        | [] ->
            Vec.set t.pcs pc_idx else_;
            guard else_
        | [ (instr, _) ] ->
            Vec.set t.pcs pc_idx instr;
            guard instr
        | l -> Error (`Select_multiple_guards_true (pc, List.map l ~f:snd)))
    | Read (dst_id, chan) ->
        (* unguard pc; *)
        if chan.read_ready then
          Error (`Simul_chan_readers (chan.read_instr, pc))
        else (
          chan.read_ready <- true;
          chan.read_instr <- pc;
          chan.read_dst_var_id <- dst_id;
          Vec.remove t.pcs pc_idx;
          step_chan chan)
    | Send (expr, chan) ->
        (* unguard pc; *)
        if chan.send_ready then
          Error (`Simul_chan_senders (chan.send_instr, pc))
        else (
          chan.send_ready <- true;
          chan.send_instr <- pc;
          chan.send_expr <- expr;
          Vec.remove t.pcs pc_idx;
          step_chan chan)
    | Send_enqueuer d ->
        unguard pc;
        assert (not d.is_done);
        if Queue.is_empty d.to_send then (
          Vec.remove t.pcs pc_idx;
          d.is_done <- true;
          Ok ())
        else (
          Var_table.set t.var_table ~var_id:d.var_id
            ~value:(Queue.dequeue_exn d.to_send);
          Vec.set t.pcs pc_idx (pc - 1);
          guard (pc - 1))
    | Read_dequeuer d ->
        unguard pc;
        let value = Var_table.at t.var_table ~var_id:d.var_id in
        let expected = Queue.dequeue_exn d.expected_reads in
        if not (d.equals value expected) then
          Error (`Read_dequeuer_wrong_value (pc, value, expected))
        else if Queue.is_empty d.expected_reads then (
          Vec.remove t.pcs pc_idx;
          Ok ())
        else (
          Vec.set t.pcs pc_idx (pc - 1);
          guard (pc - 1))

  let step t =
    if Vec.is_empty t.pcs then Error `Stuck
    else step' t ~pc_idx:(Random.State.int_incl t.rng 0 (Vec.length t.pcs - 1))

  let find_rw t var_id ~ignore ~get_ids =
    let is_rw pc =
      (not (Var_id.equal ignore pc)) && Set.mem (get_ids t.assem.(pc)) var_id
    in
    let rw_id_of_chan (chan : Chan_buff.t) =
      let send_i, read_i = (chan.send_instr, chan.read_instr) in
      if chan.send_ready && is_rw send_i then Some send_i
      else if chan.read_ready && is_rw read_i then Some read_i
      else None
    in
    Option.value_exn
      (match List.find_map t.chan_buffs ~f:rw_id_of_chan with
      | Some pc -> Some pc
      | None -> Vec.find t.pcs ~f:is_rw)

  let find_reader t var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.read_ids

  let find_writer t var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.write_ids

  let resolve_step_err t e ~step_idx =
    match e with
    | `Stuck -> Wait_outcome.Stuck step_idx
    | `Uninit_id (var_id, pc) -> Uninit_id (var_id, pc)
    | `Simul_chan_senders (fst_pc, snd_pc) -> Simul_chan_senders (fst_pc, snd_pc)
    | `Simul_chan_readers (fst_pc, snd_pc) -> Simul_chan_readers (fst_pc, snd_pc)
    | `Assert_failure pc -> Assert_failure pc
    | `Reading_written_var (var_id, pc) ->
        Simul_read_write (var_id, pc, find_writer t var_id ~ignore:pc)
    | `Writing_read_var (var_id, pc) ->
        Simul_read_write (var_id, find_reader t var_id ~ignore:pc, pc)
    | `Writing_written_var (var_id, pc) ->
        Simul_write_write (var_id, pc, find_writer t var_id ~ignore:pc)
    | `Select_no_guards_true pc -> Select_no_guards_true pc
    | `Select_multiple_guards_true (pc, branch_idxs) ->
        Select_multiple_guards_true (branch_idxs, pc)
    | `Read_dequeuer_wrong_value (pc, expected, actual) ->
        Read_dequeuer_wrong_value (pc, expected, actual)

  let maybe_replace_with_queuers_unfinished t orig_status =
    let enq =
      Array.findi t.assem ~f:(fun _ instr ->
          match instr with Send_enqueuer d -> not d.is_done | _ -> false)
    in
    let deq =
      Array.findi t.assem ~f:(fun _ instr ->
          match instr with
          | Read_dequeuer d -> not (Queue.is_empty d.expected_reads)
          | _ -> false)
    in
    match enq with
    | Some (pc, _) -> Wait_outcome.Send_enqueuer_not_done pc
    | None -> (
        match deq with
        | Some (pc, _) -> Read_dequeuer_not_done pc
        | None -> orig_status)

  let wait ?(max_steps = 1000) t =
    let step_loop step_idx =
      match step t with
      | Ok () -> `Continue
      | Error e ->
          let status = resolve_step_err t e ~step_idx in
          let status =
            match status with
            | Wait_outcome.Stuck _ ->
                maybe_replace_with_queuers_unfinished t status
            | _ -> status
          in
          (match status with
          | Wait_outcome.Stuck _ -> ()
          | _ -> t.is_done <- true);
          `Return status
    in
    if t.is_done then Wait_outcome.Already_errored
    else
      for_loop_else max_steps ~else_:Wait_outcome.Time_out ~f:(fun step_idx ->
          step_loop step_idx)

  module Advanced = struct
    let set_user_sends t ~values ~send_instr =
      let enqueuer =
        match (t.assem.(send_instr), t.assem.(send_instr + 1)) with
        | Send _, Send_enqueuer enqueuer -> enqueuer
        | _ -> assert false
      in
      enqueuer.is_done <- false;
      queue_extend enqueuer.to_send values;
      assert (
        Result.is_ok (Var_table.guard' t.var_table t.assem (send_instr + 1)));
      Vec.push t.pcs (send_instr + 1)

    let set_user_reads t ~values ~read_instr =
      let dequeuer =
        match (t.assem.(read_instr), t.assem.(read_instr + 1)) with
        | Read _, Read_dequeuer dequeuer -> dequeuer
        | _ -> assert false
      in
      queue_extend dequeuer.expected_reads values;
      assert (Result.is_ok (Var_table.guard' t.var_table t.assem read_instr));
      Vec.push t.pcs read_instr
  end
end

let%expect_test "test successful" =
  let var0 = Var_id.of_int 0 in
  let var1 = Var_id.of_int 1 in
  let assem =
    [|
      N.Jump 1;
      Assign (var0, Expr.const 3);
      Assign (var1, Expr.const "7");
      Assign (var0, Expr.map (Var var0) ~f:Int.to_string);
      Par [ 5; 7 ];
      Log (Var var0);
      Jump 8;
      Log (Var var1);
      ParJoin (Par_join.create ~max_ct:2);
      End;
    |]
  in
  let sim = Sim.create assem ~var_ct:3 in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {| 73(Stuck 11) |}]

let%expect_test "error assertion failure" =
  let var0 = Var_id.of_int 0 in
  let assem =
    [|
      N.Jump 1;
      Assign (var0, Expr.const 3);
      Assert (Expr.map (Var var0) ~f:(fun v -> Int.equal v 3));
      Assert (Expr.map (Var var0) ~f:(fun v -> Int.equal v 4));
      Assert (Expr.map (Var var0) ~f:(fun v -> Int.equal v 5));
    |]
  in
  let sim = Sim.create assem ~var_ct:3 in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {| (Assert_failure 3) |}]

let%expect_test "error uninitialized read" =
  let var0 = Var_id.of_int 0 in
  let var1 = Var_id.of_int 1 in
  let assem =
    [|
      N.Jump 1;
      Assign (var0, Expr.const 3);
      Assert (Expr.map (Var var0) ~f:(fun v -> Int.equal v 3));
      Assert (Expr.map (Var var1) ~f:(fun v -> Int.equal v 4));
      Assert (Expr.map (Var var0) ~f:(fun v -> Int.equal v 5));
    |]
  in
  let sim = Sim.create assem ~var_ct:3 in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {| (Uninit_id 1 3) |}]

let%expect_test "error simultanious write write" =
  let var0 = Var_id.of_int 0 in
  let var1 = Var_id.of_int 1 in
  let assem =
    [|
      N.Jump 1;
      Par [ 2; 6 ];
      Assign (var1, Expr.const 3);
      Assign (var0, Expr.const 3);
      Assign (var0, Expr.const 3);
      Jump 9;
      Assign (var0, Expr.const 3);
      Assign (var0, Expr.const 3);
      Assign (var0, Expr.const 3);
      ParJoin (Par_join.create ~max_ct:2);
      End;
    |]
  in
  let sim = Sim.create assem ~var_ct:3 in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {| (Simul_write_write 0 3 6) |}]

let%expect_test "error simultanious read write" =
  let var0 = Var_id.of_int 0 in
  let var1 = Var_id.of_int 1 in
  let var2 = Var_id.of_int 2 in
  let assem =
    [|
      N.Jump 1;
      Assign (var0, Expr.const 3);
      Par [ 3; 6 ];
      Assign (var1, Expr.const 3);
      Assign (var0, Expr.const 3);
      Jump 9;
      Assign (var2, Var var0);
      Assign (var2, Var var0);
      Assign (var2, Var var0);
      ParJoin (Par_join.create ~max_ct:2);
      End;
    |]
  in
  let sim = Sim.create assem ~var_ct:3 in
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {| (Simul_read_write 0 6 4) |}]
