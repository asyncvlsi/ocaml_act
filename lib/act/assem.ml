open! Core
module Var_id = Int

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

module N = struct
  type t =
    | End
    | Unreachable
    | ConsumePC
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
  [@@deriving sexp]

  let read_ids t =
    (match t with
    | End | Unreachable | ConsumePC | Par _ | ParJoin _ | Jump _ -> []
    | Assign (_, expr) -> Expr.var_ids expr
    | Log expr | Assert expr | JumpIfFalse (expr, _) -> Expr.var_ids expr
    | SelectImm l | SelectImmElse (l, _) ->
        List.concat_map l ~f:(fun (expr, _) -> Expr.var_ids expr)
    | Read (_, _) -> []
    | Send (expr, _) -> Expr.var_ids expr)
    |> Var_id.Set.of_list

  let write_ids t =
    (match t with
    | End | Unreachable | ConsumePC | Par _ | ParJoin _ | Jump _ -> []
    | Assign (id, _) -> [ id ]
    | Log _ | Assert _ | JumpIfFalse (_, _) -> []
    | SelectImm _ -> []
    | SelectImmElse _ -> []
    | Read (var_id, _) -> [ var_id ]
    | Send (_, _) -> [])
    |> Var_id.Set.of_list

  let var_ids t = Set.union (read_ids t) (write_ids t) |> Set.to_list
end

module Builder = struct
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

  let check_eval t ~read_ids ~write_ids =
    match Set.find read_ids ~f:(fun read_id -> t.write_cts.(read_id) > 0) with
    | Some read_id -> `Reading_written_var read_id
    | None -> (
        match
          Set.find read_ids ~f:(fun read_id -> not t.is_inited.(read_id))
        with
        | Some read_id -> `Uninit_id read_id
        | None -> (
            match
              Set.find write_ids ~f:(fun write_id -> t.read_cts.(write_id) > 0)
            with
            | Some write_id -> `Writing_read_var write_id
            | None -> (
                match
                  Set.find write_ids ~f:(fun write_id ->
                      t.write_cts.(write_id) > 0)
                with
                | Some write_id -> `Writing_written_var write_id
                | None -> `Success)))

  let rec eval t expr =
    match expr with
    | Expr.Var id -> t.values.(id)
    | Const c -> c
    | Map (e, f) -> f (eval t e)

  let guard t ~read_ids ~write_ids =
    (* print_s [%sexp (("guarding", read_ids, write_ids): (string * Var_id.Set.t  * Var_id.Set.t ))]; *)
    let status = check_eval t ~read_ids ~write_ids in
    match status with
    | `Success ->
        Set.iter read_ids ~f:(fun read_id ->
            t.read_cts.(read_id) <- t.read_cts.(read_id) + 1);
        Set.iter write_ids ~f:(fun write_id ->
            t.write_cts.(write_id) <- t.write_cts.(write_id) + 1);
        `Success
    | _ -> status

  let unguard t ~read_ids ~write_ids =
    Set.iter read_ids ~f:(fun read_id ->
        t.read_cts.(read_id) <- t.read_cts.(read_id) - 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.write_cts.(write_id) <- t.write_cts.(write_id) - 1)

  let set t ~var_id ~value =
    t.values.(var_id) <- value;
    t.is_inited.(var_id) <- true

  let assign t ~var_id ~expr = set t ~var_id ~value:(eval t expr)
end

let for_loop_else max_ct ~f ~else_ =
  let return = ref None in
  let ct = ref 0 in
  while !ct < max_ct && Option.is_none !return do
    (match f ct with `Continue -> () | `Return d -> return := Some d);
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
      | Done
      | Already_done
      | Time_out
      | Stuck
      | Assert_failure of Instr_idx.t
      | Uninit_id of Var_id.t * Instr_idx.t
      | Simul_chan_senders of Instr_idx.t * Instr_idx.t
      | Simul_chan_readers of Instr_idx.t * Instr_idx.t
      | Simul_read_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Simul_write_write of Var_id.t * Instr_idx.t * Instr_idx.t
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
    let unguard pc =
      Var_table.unguard t.var_table
        ~read_ids:(N.read_ids t.assem.(pc))
        ~write_ids:(N.write_ids t.assem.(pc))
    in
    let guard pc =
      match
        Var_table.guard t.var_table
          ~read_ids:(N.read_ids t.assem.(pc))
          ~write_ids:(N.write_ids t.assem.(pc))
      with
      | `Success -> `Success
      | `Uninit_id var_id -> `Uninit_id (var_id, pc)
      | `Reading_written_var var_id -> `Reading_written_var (var_id, pc)
      | `Writing_read_var var_id -> `Writing_read_var (var_id, pc)
      | `Writing_written_var var_id -> `Writing_written_var (var_id, pc)
    in
    let step_chan (chan : Chan_buff.t) =
      if chan.read_ready && chan.send_ready then (
        chan.read_ready <- false;
        chan.send_ready <- false;
        unguard chan.read_instr;
        unguard chan.send_instr;
        Var_table.assign t.var_table ~var_id:chan.read_dst_var_id
          ~expr:chan.send_expr;
        Vec.extend t.pcs [ chan.read_instr + 1; chan.send_instr + 1 ];
        unguard (chan.read_instr + 1);
        unguard (chan.send_instr + 1);
        `Success)
      else `Success
    in
    let pc = Vec.at t.pcs pc_idx in
    match t.assem.(pc) with
    | End ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        if not (Vec.is_empty t.pcs) then
          failwith
            "somehow reached `End` while there are still program counters. The \
             provided assem is invalid.";
        `Done
    | ConsumePC ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        `Success
    | Unreachable -> failwith "reached an 'unreachable' instruction"
    | Assign (var_id, expr) ->
        unguard pc;
        (* print_s [%sexp (("after_ungaurd_assign", t.var_table) :string *  Var_table.t)]; *)
        Var_table.assign t.var_table ~var_id ~expr;
        vec_incr t.pcs pc_idx;
        (* print_s [%sexp (("before_gaurd_assign", t.var_table) :string *  Var_table.t)]; *)
        guard (pc + 1)
    | Assert expr ->
        unguard pc;
        if Obj.magic (Var_table.eval t.var_table expr) then (
          vec_incr t.pcs pc_idx;
          guard (pc + 1))
        else `Assert_failure pc
    | Log expr ->
        unguard pc;
        printf "%s" (Obj.magic (Var_table.eval t.var_table expr));
        vec_incr t.pcs pc_idx;
        guard (pc + 1)
    | Par instrs -> (
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        Vec.extend t.pcs instrs;
        match
          List.find_map instrs ~f:(fun instr ->
              let status = guard instr in
              match status with `Success -> None | _ -> Some status)
        with
        | None -> `Success
        | Some status -> status)
    | ParJoin d ->
        (* unguard pc; *)
        d.curr_ct <- d.curr_ct + 1;
        if Int.equal d.max_ct d.curr_ct then (
          d.curr_ct <- 0;
          vec_incr t.pcs pc_idx;
          guard (pc + 1))
        else (
          Vec.remove t.pcs pc_idx;
          `Success)
    | Jump inst ->
        (* unguard pc; *)
        Vec.set t.pcs pc_idx inst;
        guard inst
    | JumpIfFalse (expr, inst) -> (
        unguard pc;
        match Obj.magic (Var_table.eval t.var_table expr) with
        | true ->
            vec_incr t.pcs pc_idx;
            guard (pc + 1)
        | false ->
            Vec.set t.pcs pc_idx inst;
            guard inst)
    | SelectImm l -> (
        unguard pc;
        match
          List.filter_mapi l ~f:(fun idx (expr, instr) ->
              if Obj.magic (Var_table.eval t.var_table expr) then
                Some (instr, idx)
              else None)
        with
        | [] -> `Select_no_guards_true pc
        | [ (instr, _) ] ->
            Vec.set t.pcs pc_idx instr;
            guard instr
        | l -> `Select_multiple_guards_true (pc, List.map l ~f:snd))
    | SelectImmElse (l, else_) -> (
        unguard pc;
        match
          List.filter_mapi l ~f:(fun idx (expr, instr) ->
              if Obj.magic (Var_table.eval t.var_table expr) then
                Some (instr, idx)
              else None)
        with
        | [] ->
            Vec.set t.pcs pc_idx else_;
            guard else_
        | [ (instr, _) ] ->
            Vec.set t.pcs pc_idx instr;
            guard instr
        | l -> `Select_multiple_guards_true (pc, List.map l ~f:snd))
    | Read (dst_id, chan) ->
        (* unguard pc; *)
        if chan.read_ready then `Simul_chan_readers (chan.read_instr, pc)
        else (
          chan.read_ready <- true;
          chan.read_instr <- pc;
          chan.read_dst_var_id <- dst_id;
          Vec.remove t.pcs pc_idx;
          (* guard pc; *)
          step_chan chan)
    | Send (expr, chan) ->
        (* unguard pc; *)
        if chan.send_ready then `Simul_chan_senders (chan.send_instr, pc)
        else (
          chan.send_ready <- true;
          chan.send_instr <- pc;
          chan.send_expr <- expr;
          Vec.remove t.pcs pc_idx;
          (* guard pc; *)
          step_chan chan)

  let step t =
    (* print_s [%sexp (("start_of_step", t.var_table) :string *  Var_table.t)]; *)
    if Vec.is_empty t.pcs then `Stuck
    else
      let pc_idx = Random.State.int_incl t.rng 0 (Vec.length t.pcs - 1) in
      step' t ~pc_idx

  let find_rw t var_id ~ignore ~get_ids =
    let helper pc =
      if (not (Var_id.equal ignore pc)) && Set.mem (get_ids t.assem.(pc)) var_id
      then Some pc
      else None
    in
    let pc =
      match Vec.find_map t.pcs ~f:helper with
      | Some pc -> Some pc
      | None ->
          List.find_map t.chan_buffs ~f:(fun chan ->
              let s =
                if chan.send_ready then helper chan.send_instr else None
              in
              let r =
                if chan.read_ready then helper chan.read_instr else None
              in
              match (s, r) with
              | Some pc, _ | _, Some pc -> Some pc
              | None, None -> None)
    in
    match pc with Some pc -> pc | None -> failwith "could not find a reader"

  let find_reader t var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.read_ids

  let find_writer t var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.write_ids

  let wait ?(max_steps = 1000) t =
    if t.is_done then Wait_outcome.Already_done
    else
      let status =
        for_loop_else max_steps ~else_:Wait_outcome.Time_out ~f:(fun _ ->
            let res = step t in
            match res with
            | `Success -> `Continue
            | `Done -> `Return Wait_outcome.Done
            | `Stuck -> `Return Stuck
            | `Uninit_id (var_id, pc) -> `Return (Uninit_id (var_id, pc))
            | `Simul_chan_senders (fst_pc, snd_pc) ->
                `Return (Simul_chan_senders (fst_pc, snd_pc))
            | `Simul_chan_readers (fst_pc, snd_pc) ->
                `Return (Simul_chan_readers (fst_pc, snd_pc))
            | `Assert_failure pc -> `Return (Assert_failure pc)
            (* TODO decode these! *)
            | `Reading_written_var (var_id, pc) ->
                `Return
                  (Simul_read_write (var_id, pc, find_writer t var_id ~ignore:pc))
            | `Writing_read_var (var_id, pc) ->
                `Return
                  (Simul_read_write (var_id, find_reader t var_id ~ignore:pc, pc))
            | `Writing_written_var (var_id, pc) ->
                `Return
                  (Simul_write_write
                     (var_id, pc, find_writer t var_id ~ignore:pc))
            | `Select_no_guards_true _ -> failwith "TODO"
            | `Select_multiple_guards_true _ -> failwith "TODO")
      in
      (match status with
      | Wait_outcome.Time_out | Wait_outcome.Stuck -> ()
      | _ -> t.is_done <- true);
      status

  module Advanced = struct
    let add_pc t pc = Vec.push t.pcs pc
    let set_var t ~var_id ~value = Var_table.set t.var_table ~var_id ~value
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
  [%expect {| 73Done |}]

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
