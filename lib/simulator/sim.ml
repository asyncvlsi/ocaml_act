open! Core
open! Act
module Ir = Internal_rep

module Var_id = struct
  include Int
end

let queue_extend q l = List.iter l ~f:(fun e -> Queue.enqueue q e)

let for_loop_else max_ct ~(f : int -> [ `Continue | `Return of 'a ])
    ~(else_ : 'a) =
  let return = ref None in
  let ct = ref 0 in
  while !ct < max_ct && Option.is_none !return do
    (match f !ct with `Continue -> () | `Return d -> return := Some d);
    incr ct
  done;
  Option.value !return ~default:else_

let some_or_thunk o ~f = match o with Some v -> Some v | None -> f ()

module Expr = struct
  type t =
    | Var of Var_id.t
    | Const of Any.t
    | Map of t * (Any.t -> Any.t)
    | Map2 of t * t * (Any.t -> Any.t -> Any.t)
  [@@deriving sexp_of]

  let sexp_of_t _ = String.sexp_of_t "<expr>"
  let map e ~f = Map (e, Obj.magic f)
  let map2 e1 e2 ~f = Map2 (e1, e2, Obj.magic f)

  let rec var_ids t =
    match t with
    | Var id -> [ id ]
    | Const _ -> []
    | Map (e, _) -> var_ids e
    | Map2 (e1, e2, _) -> var_ids e1 @ var_ids e2
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
  [@@deriving sexp_of]

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

module Mem_buff = struct
  type t = { arr : Any.t array; idx_helper_reg : Var_id.t } [@@deriving sexp_of]

  let create ~init ~idx_helper_reg = { arr = Array.copy init; idx_helper_reg }
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
    mutable idx : int;
  }
  [@@deriving sexp_of]

  let create ~var_id =
    { to_send = Queue.create (); var_id; is_done = true; idx = 0 }
end

module Read_dequeuer = struct
  type t = {
    expected_reads : Any.t Queue.t;
    var_id : Var_id.t;
    equals : (Any.t -> Any.t -> bool[@sexp.opaque]);
    mutable idx : int;
  }
  [@@deriving sexp_of]

  let create ~var_id ~equals =
    { expected_reads = Queue.create (); var_id; equals; idx = 0 }
end

module N = struct
  type t =
    | End
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
    | ReadMem of (* idx *) Expr.t * (* dst *) Var_id.t * Mem_buff.t
    | WriteMem of (* idx *) Expr.t * (* src *) Expr.t * Mem_buff.t
    (* These are ``magic'' instructions that allow user io operations. These instruction
       should be placed immediatly after the assoiated send/read instruction *)
    | Send_enqueuer of Send_enqueuer.t
    | Read_dequeuer of Read_dequeuer.t
  [@@deriving sexp_of]

  let read_ids t =
    (match t with
    | End | Nop | Par _ | ParJoin _ -> []
    | Read (_, _) | Jump _ -> []
    | Assign (_, expr) -> Expr.var_ids expr
    | Log expr | Assert expr | JumpIfFalse (expr, _) -> Expr.var_ids expr
    | SelectImm l | SelectImmElse (l, _) ->
        List.concat_map l ~f:(fun (expr, _) -> Expr.var_ids expr)
    | Send (expr, _) -> Expr.var_ids expr
    | Send_enqueuer _ -> []
    | Read_dequeuer d -> [ d.var_id ]
    | ReadMem (idx_expr, _, _) -> Expr.var_ids idx_expr
    | WriteMem (idx_expr, src_expr, _) ->
        Expr.var_ids idx_expr @ Expr.var_ids src_expr)
    |> Var_id.Set.of_list

  let write_ids t =
    (match t with
    | End | Nop | Par _ | ParJoin _ -> []
    | Log _ | Assert _ | JumpIfFalse (_, _) | Jump _ -> []
    | SelectImm _ | SelectImmElse _ | Send (_, _) -> []
    | Assign (id, _) -> [ id ]
    | Read (var_id, _) -> [ var_id ]
    | Send_enqueuer d -> [ d.var_id ]
    | Read_dequeuer _ -> []
    | ReadMem (_, dst, mem) -> [ dst; mem.idx_helper_reg ]
    | WriteMem (_, _, mem) -> [ mem.idx_helper_reg ])
    |> Var_id.Set.of_list
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
  [@@deriving sexp_of]

  let create var_ct var_inits =
    let max_id =
      (Hashtbl.keys var_inits
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0)
      + 1
    in
    assert (max_id < var_ct);
    let values = Array.init var_ct ~f:(fun _ -> Obj.magic 0) in
    let is_inited = Array.init var_ct ~f:(fun _ -> false) in
    Hashtbl.iteri var_inits ~f:(fun ~key:var_id ~data:value ->
        match value with
        | Some value ->
            values.(var_id) <- value;
            is_inited.(var_id) <- true
        | None -> is_inited.(var_id) <- false);
    {
      values;
      is_inited;
      read_cts = Array.init var_ct ~f:(fun _ -> 0);
      write_cts = Array.init var_ct ~f:(fun _ -> 0);
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
    | Map2 (e1, e2, f) -> f (eval t e1) (eval t e2)

  let eval_int t expr = eval t expr |> Any.to_int
  let eval_bool t expr = eval t expr |> Any.to_bool
  let eval_string t expr = eval t expr |> Any.to_string

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
    let i = assem.(pc) in
    guard t ~read_ids:(N.read_ids i) ~write_ids:(N.write_ids i) ~pc

  let unguard' t assem pc =
    let i = assem.(pc) in
    unguard t ~read_ids:(N.read_ids i) ~write_ids:(N.write_ids i)

  let at t ~var_id =
    assert t.is_inited.(var_id);
    t.values.(var_id)

  let set t ~var_id ~value =
    t.values.(var_id) <- value;
    t.is_inited.(var_id) <- true

  let assign t ~var_id ~expr = set t ~var_id ~value:(eval t expr)
end

module ASim = struct
  type t = {
    assem : N.t array;
    mutable pcs : Instr_idx.t Vec.t;
    mutable var_table : Var_table.t;
    rng : (Random.State.t[@sexp.opaque]);
    mutable is_done : bool;
    chan_buffs : Chan_buff.t list;
    all_enqueuers : (Instr_idx.t * Send_enqueuer.t) list;
    all_dequeuers : (Instr_idx.t * Read_dequeuer.t) list;
  }
  [@@deriving sexp_of]

  module Wait_error = struct
    type t =
      | Already_errored
      | Time_out
      | Assert_failure of Instr_idx.t
      | Uninit_id of Var_id.t * Instr_idx.t
      | Simul_chan_senders of Instr_idx.t * Instr_idx.t
      | Simul_chan_readers of Instr_idx.t * Instr_idx.t
      | Simul_read_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Simul_write_write of Var_id.t * Instr_idx.t * Instr_idx.t
      | Select_no_guards_true of Instr_idx.t
      | Select_multiple_guards_true of int list * Instr_idx.t
      | Read_dequeuer_wrong_value of Instr_idx.t * Any.t * Any.t * int
      | Read_dequeuer_not_done of Instr_idx.t * int
      | Send_enqueuer_not_done of Instr_idx.t * int
      | Mem_out_of_bounds of Instr_idx.t * int * int
    [@@deriving sexp_of]
  end

  let create ?(seed = 0) assem ~var_ct ~var_inits =
    (match assem.(0) with
    | N.Jump _ -> ()
    | _ ->
        failwith
          "The first assembly instruction must be a jump. This ensures it \
           neither reads nor writes any values");
    let var_table = Var_table.create var_ct var_inits in
    let pcs = Vec.of_array [| 0 |] ~default:(-1) in
    let rng = Random.State.make [| seed |] in
    let chan_buffs =
      List.filter_map (Array.to_list assem) ~f:(function
        | Send (_, chan) | Read (_, chan) -> Some chan
        | _ -> None)
    in
    let all_enqueuers =
      List.filter_mapi (Array.to_list assem) ~f:(fun pc instr ->
          match instr with
          | Send_enqueuer enqueuer -> Some (pc, enqueuer)
          | _ -> None)
    in
    let all_dequeuers =
      List.filter_mapi (Array.to_list assem) ~f:(fun pc instr ->
          match instr with
          | Read_dequeuer dequeuer -> Some (pc, dequeuer)
          | _ -> None)
    in
    {
      assem;
      pcs;
      var_table;
      rng;
      is_done = false;
      chan_buffs;
      all_enqueuers;
      all_dequeuers;
    }

  let step' t ~pc_idx =
    let guard pc = Var_table.guard' t.var_table t.assem pc in
    let set_pc_and_guard ~pc_idx new_pc =
      Vec.set t.pcs pc_idx new_pc |> fun () -> guard new_pc
    in
    let push_pc_and_guard new_pc =
      Vec.push t.pcs new_pc |> fun () -> guard new_pc
    in
    let eval_bool expr = Var_table.eval_bool t.var_table expr in
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
    let step_select l ~else_ ~pc ~pc_idx =
      let true_branches =
        List.mapi l ~f:(fun idx (expr, instr) -> (eval_bool expr, instr, idx))
        |> List.filter ~f:(fun (g, _, _) -> g)
        |> List.map ~f:(fun (_, instr, idx) -> (instr, idx))
      in
      match (true_branches, else_) with
      | [], None -> Error (`Select_no_guards_true pc)
      | [], Some else_ -> set_pc_and_guard ~pc_idx else_
      | [ (instr, _) ], _ -> set_pc_and_guard ~pc_idx instr
      | l, _ -> Error (`Select_multiple_guards_true (pc, List.map l ~f:snd))
    in
    let pc = Vec.at t.pcs pc_idx in
    match t.assem.(pc) with
    | End ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        Ok ()
    | Nop ->
        (* unguard pc; *)
        set_pc_and_guard ~pc_idx (pc + 1)
    | Assign (var_id, expr) ->
        unguard pc;
        Var_table.assign t.var_table ~var_id ~expr;
        set_pc_and_guard ~pc_idx (pc + 1)
    | Assert expr -> (
        unguard pc;
        match eval_bool expr with
        | true -> set_pc_and_guard ~pc_idx (pc + 1)
        | false -> Error (`Assert_failure pc))
    | Log expr ->
        unguard pc;
        printf "%s" (Var_table.eval_string t.var_table expr);
        set_pc_and_guard ~pc_idx (pc + 1)
    | Par instrs ->
        (* unguard pc; *)
        Vec.remove t.pcs pc_idx;
        Result.all_unit (List.map instrs ~f:push_pc_and_guard)
    | ParJoin d -> (
        (* unguard pc; *)
        d.curr_ct <- d.curr_ct + 1;
        match Int.equal d.max_ct d.curr_ct with
        | true ->
            d.curr_ct <- 0;
            set_pc_and_guard ~pc_idx (pc + 1)
        | false ->
            Vec.remove t.pcs pc_idx;
            Ok ())
    | Jump inst ->
        (* unguard pc; *)
        set_pc_and_guard ~pc_idx inst
    | JumpIfFalse (expr, inst) ->
        unguard pc;
        set_pc_and_guard ~pc_idx (if eval_bool expr then pc + 1 else inst)
    | SelectImm l ->
        unguard pc;
        step_select l ~else_:None ~pc ~pc_idx
    | SelectImmElse (l, else_) ->
        unguard pc;
        step_select l ~else_:(Some else_) ~pc ~pc_idx
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
    | Send_enqueuer d -> (
        unguard pc;
        assert (not d.is_done);
        match Queue.dequeue d.to_send with
        | None ->
            Vec.remove t.pcs pc_idx;
            d.is_done <- true;
            Ok ()
        | Some value ->
            d.idx <- d.idx + 1;
            Var_table.set t.var_table ~var_id:d.var_id ~value;
            set_pc_and_guard ~pc_idx (pc - 1))
    | Read_dequeuer d ->
        unguard pc;
        let value = Var_table.at t.var_table ~var_id:d.var_id in
        let expected = Queue.dequeue_exn d.expected_reads in
        if not (d.equals value expected) then
          Error (`Read_dequeuer_wrong_value (pc, value, expected, d.idx))
        else if Queue.is_empty d.expected_reads then (
          Vec.remove t.pcs pc_idx;
          Ok ())
        else (
          d.idx <- d.idx + 1;
          set_pc_and_guard ~pc_idx (pc - 1))
    | ReadMem (idx_expr, dst_id, mem) ->
        unguard pc;
        let idx = Var_table.eval_int t.var_table idx_expr in
        if idx < 0 || idx >= Array.length mem.arr then
          Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
        else (
          Var_table.set t.var_table ~var_id:dst_id ~value:mem.arr.(idx);
          set_pc_and_guard ~pc_idx (pc + 1))
    | WriteMem (idx_expr, src_expr, mem) ->
        unguard pc;
        let idx = Var_table.eval_int t.var_table idx_expr in
        if idx < 0 || idx >= Array.length mem.arr then
          Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
        else
          let value = Var_table.eval t.var_table src_expr in
          mem.arr.(idx) <- value;
          set_pc_and_guard ~pc_idx (pc + 1)

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
    List.find_map t.chan_buffs ~f:rw_id_of_chan
    |> some_or_thunk ~f:(fun () -> Vec.find t.pcs ~f:is_rw)
    |> Option.value_exn

  let resolve_step_err t e =
    let find_reader var_id ~ignore =
      find_rw t var_id ~ignore ~get_ids:N.read_ids
    in
    let find_writer var_id ~ignore =
      find_rw t var_id ~ignore ~get_ids:N.write_ids
    in
    match e with
    | `Stuck -> Ok ()
    | `Uninit_id (var_id, pc) -> Error (Wait_error.Uninit_id (var_id, pc))
    | `Simul_chan_senders (fst_pc, snd_pc) ->
        Error (Simul_chan_senders (fst_pc, snd_pc))
    | `Simul_chan_readers (fst_pc, snd_pc) ->
        Error (Simul_chan_readers (fst_pc, snd_pc))
    | `Assert_failure pc -> Error (Assert_failure pc)
    | `Reading_written_var (var_id, pc) ->
        Error (Simul_read_write (var_id, pc, find_writer var_id ~ignore:pc))
    | `Writing_read_var (var_id, pc) ->
        Error (Simul_read_write (var_id, find_reader var_id ~ignore:pc, pc))
    | `Writing_written_var (var_id, pc) ->
        Error (Simul_write_write (var_id, pc, find_writer var_id ~ignore:pc))
    | `Select_no_guards_true pc -> Error (Select_no_guards_true pc)
    | `Select_multiple_guards_true (pc, branch_idxs) ->
        Error (Select_multiple_guards_true (branch_idxs, pc))
    | `Read_dequeuer_wrong_value (pc, expected, actual, idx) ->
        Error (Read_dequeuer_wrong_value (pc, expected, actual, idx))
    | `Mem_out_of_bounds (pc, idx, len) ->
        Error (Mem_out_of_bounds (pc, idx, len))

  let queuers_unfinished_check t =
    match
      List.find t.all_dequeuers ~f:(fun (_, d) ->
          not (Queue.is_empty d.expected_reads))
    with
    | Some (pc, deq) -> Error (Wait_error.Read_dequeuer_not_done (pc, deq.idx))
    | None -> (
        match List.find t.all_enqueuers ~f:(fun (_, d) -> not d.is_done) with
        | Some (pc, enq) -> Error (Send_enqueuer_not_done (pc, enq.idx))
        | None -> Ok ())

  let schedual_user_chan_ops t ~user_sends ~user_reads =
    let set_user_sends t ~values ~send_instr =
      let enqueuer =
        match (t.assem.(send_instr), t.assem.(send_instr + 1)) with
        | Send _, Send_enqueuer enqueuer -> enqueuer
        | _ -> assert false
      in
      enqueuer.is_done <- false;
      enqueuer.idx <- 0;
      queue_extend enqueuer.to_send values;
      assert (
        Result.is_ok (Var_table.guard' t.var_table t.assem (send_instr + 1)));
      Vec.push t.pcs (send_instr + 1)
    in
    let set_user_reads t ~values ~read_instr =
      let dequeuer =
        match (t.assem.(read_instr), t.assem.(read_instr + 1)) with
        | Read _, Read_dequeuer dequeuer -> dequeuer
        | _ -> assert false
      in
      dequeuer.idx <- 0;
      queue_extend dequeuer.expected_reads values;
      assert (Result.is_ok (Var_table.guard' t.var_table t.assem read_instr));
      Vec.push t.pcs read_instr
    in
    Map.iteri user_sends ~f:(fun ~key:send_instr ~data:values ->
        set_user_sends t ~values ~send_instr);
    Map.iteri user_reads ~f:(fun ~key:read_instr ~data:values ->
        set_user_reads t ~values ~read_instr)

  let wait ?(max_steps = 1000) t ~user_sends ~user_reads =
    let step_loop _ =
      match step t with
      | Ok () -> `Continue
      | Error e ->
          let status =
            resolve_step_err t e
            |> Result.bind ~f:(fun () -> queuers_unfinished_check t)
          in
          Result.iter_error status ~f:(fun _ -> t.is_done <- true);
          `Return status
    in
    match t.is_done with
    | false ->
        schedual_user_chan_ops t ~user_sends ~user_reads;
        for_loop_else max_steps
          ~f:(fun step_idx -> step_loop step_idx)
          ~else_:(Error Time_out)
    | true -> Error Already_errored
end

module Assem_builder = struct
  type t = {
    assem : Assem_vec.t;
    mutable next_assem_var_id : int;
    var_id_to_assem_id : Var_id.t Ir.Var.U.Table.t;
  }

  let create () =
    {
      assem = Assem_vec.create ();
      next_assem_var_id = 0;
      var_id_to_assem_id = Ir.Var.U.Table.create ();
    }

  let push t instr = Assem_vec.push t.assem instr
  let edit t idx assem = Assem_vec.edit t.assem idx assem
  let next_idx t = Assem_vec.next_idx t.assem
  let assem_array t = Assem_vec.to_array t.assem

  let new_assem_var_id t =
    let id = t.next_assem_var_id in
    t.next_assem_var_id <- t.next_assem_var_id + 1;
    Var_id.of_int id

  let to_assem_id t var_id =
    Hashtbl.find_or_add t.var_id_to_assem_id var_id ~default:(fun () ->
        new_assem_var_id t)

  let to_assem_expr t expr =
    let rec convert : 'a. 'a Ir.Expr.t -> Expr.t =
      let imap2 a b f = Expr.map2 (convert a) (convert b) ~f in
      fun (type a) (x : a Ir.Expr.t) ->
        match x with
        | Ir.Expr.Var var_id -> Expr.Var (to_assem_id t var_id.u)
        | Const c -> Const (Any.of_magic c)
        | Map (v, f) -> Expr.map (convert v) ~f
        | Add (a, b) -> imap2 a b Int.( + )
        | Sub (a, b) -> imap2 a b Int.( - )
        | Mul (a, b) -> imap2 a b Int.( * )
        | Div (a, b) -> imap2 a b Int.( / )
        | Mod (a, b) -> imap2 a b Int.( % )
        | LShift (a, b) -> imap2 a b Int.shift_left
        | LogicalRShift (a, b) -> imap2 a b Int.shift_right
        | ArithRShift (a, b) -> imap2 a b Int.shift_right_logical
        | BitAnd (a, b) -> imap2 a b Int.bit_and
        | BitOr (a, b) -> imap2 a b Int.bit_or
        | BitXor (a, b) -> imap2 a b Int.bit_xor
        | Eq (a, b) -> imap2 a b Int.equal
        | Ne (a, b) -> imap2 a b (fun a b -> not (Int.equal a b))
        | Not a -> Expr.map (convert a) ~f:not
    in
    convert expr

  let assem_var_ct t = t.next_assem_var_id

  let var_of_assem_id t =
    Hashtbl.to_alist t.var_id_to_assem_id
    |> List.map ~f:(fun (var_id, assem_id) -> (assem_id, var_id))
    |> Var_id.Table.of_alist_exn
end

type t = {
  sim : ASim.t;
  read_instr_of_chan : (* read_instr : *) Instr_idx.t Ir.Chan.U.Table.t;
  send_instr_of_chan : (* send_instr : *) Instr_idx.t Ir.Chan.U.Table.t;
  code_pos_of_instr : Code_pos.t Instr_idx.Table.t;
  var_of_assem_id : Ir.Var.U.t Var_id.Table.t;
  mutable to_send_at_next_wait : (Ir.Chan.U.t * (Any.t * Code_pos.t)) Queue.t;
  mutable to_read_at_next_wait : (Ir.Chan.U.t * (Any.t * Code_pos.t)) Queue.t;
  mutable is_done : bool;
}

let chan_of_instr mp instr =
  Hashtbl.to_alist mp
  |> List.find_exn ~f:(fun (_, i) -> Instr_idx.(equal (next i) instr))
  |> fst

let chan_of_read_instr t instr = chan_of_instr t.read_instr_of_chan instr
let chan_of_send_instr t instr = chan_of_instr t.send_instr_of_chan instr

let assem_of_ir t =
  let module Instr_idx = Instr_idx in
  let module AB = Assem_builder in
  let ab = AB.create () in
  let code_pos_of_instr = Instr_idx.Table.create () in
  let push loc instr =
    let idx = AB.push ab instr in
    Hashtbl.set code_pos_of_instr ~key:idx ~data:loc;
    idx
  in
  let edit loc idx instr =
    AB.edit ab idx instr;
    Hashtbl.set code_pos_of_instr ~key:idx ~data:loc
  in
  let chan_tbl = Ir.Chan.U.Table.create () in
  let get_chan (chan_id : Ir.Chan.U.t) =
    Hashtbl.find_or_add chan_tbl chan_id ~default:(fun () ->
        Chan_buff.create ())
  in
  let mem_tbl = Ir.Mem.Table.create () in
  let get_mem (mem : Ir.Mem.t) =
    Hashtbl.find_or_add mem_tbl mem ~default:(fun () ->
        Mem_buff.create ~init:mem.d.init
          ~idx_helper_reg:(AB.new_assem_var_id ab))
  in

  let convert_id (id : Ir.Var.U.t) = AB.to_assem_id ab id in
  let convert_expr expr = AB.to_assem_expr ab (Ir.Expr.untype expr) in

  (* Set up the initial jump. This is required by the ASim module. *)
  let () =
    let init_jump = push Code_pos.dummy_loc (Jump Instr_idx.dummy_val) in
    let start = AB.next_idx ab in
    edit Code_pos.dummy_loc init_jump (Jump start)
  in
  let rec convert stmt =
    let convert' stmt = ignore (convert stmt : Instr_idx.t) in
    let push_branches loc stmts =
      let split = push loc Nop in
      let ends =
        List.map stmts ~f:(fun stmt ->
            convert' stmt;
            push loc (Jump Instr_idx.dummy_val))
      in
      let starts =
        List.take (split :: ends) (List.length stmts)
        |> List.map ~f:Instr_idx.next
      in
      let merge = push loc Nop in
      List.iter ends ~f:(fun end_ -> edit loc end_ (Jump merge));
      (split, starts, merge)
    in
    match stmt with
    | Ir.N.Assign (loc, id, expr) ->
        push loc (Assign (convert_id id, convert_expr expr))
    | Log (loc, expr) -> push loc (Log (convert_expr expr))
    | Assert (loc, expr) -> push loc (Assert (convert_expr expr))
    | Seq (loc, stmts) -> (
        match stmts with
        | [] -> push loc Nop
        | stmts -> List.map stmts ~f:convert |> List.last_exn)
    | Par (loc, stmts) ->
        let split, starts, merge = push_branches loc stmts in
        edit loc split (Par starts);
        edit loc merge (ParJoin (Par_join.create ~max_ct:(List.length stmts)));
        merge
    | SelectImm (loc, branches, else_) -> (
        match else_ with
        | Some else_ ->
            let guards, stmts = List.unzip branches in
            let split, starts, merge = push_branches loc (else_ :: stmts) in
            let guards = List.map guards ~f:convert_expr in
            let guards = List.zip_exn guards (List.tl_exn starts) in
            edit loc split (SelectImmElse (guards, List.hd_exn starts));
            merge
        | None ->
            let guards, stmts = List.unzip branches in
            let split, starts, merge = push_branches loc stmts in
            let guards = List.map guards ~f:convert_expr in
            let guards = List.zip_exn guards starts in
            edit loc split (SelectImm guards);
            merge)
    | Read (loc, chan, var) ->
        let chan = get_chan chan in
        push loc (Read (convert_id var, chan))
    | Send (loc, chan, expr) ->
        let chan = get_chan chan in
        push loc (Send (convert_expr expr, chan))
    | Loop (loc, seq) ->
        let fst = AB.next_idx ab in
        convert' seq;
        push loc (Jump fst)
    | WhileLoop (loc, expr, seq) ->
        let split =
          push loc (JumpIfFalse (convert_expr expr, Instr_idx.dummy_val))
        in
        convert' seq;
        let jmp = push loc (Jump split) in
        edit loc split (JumpIfFalse (convert_expr expr, Instr_idx.next jmp));
        jmp
    | ReadUGMem (loc, mem, idx, dst) ->
        let mem = get_mem mem in
        push loc (ReadMem (convert_expr idx, convert_id dst, mem))
    | WriteUGMem (loc, mem, idx, value) ->
        let mem = get_mem mem in
        push loc (WriteMem (convert_expr idx, convert_expr value, mem))
  in
  let (_ : Instr_idx.t) = convert t in
  let (_ : Instr_idx.t) = push Code_pos.dummy_loc End in
  (ab, chan_tbl, code_pos_of_instr)

let create ir ~user_sendable_ports ~user_readable_ports =
  let ir = Ir.N.unwrap ir in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
  let ab, chan_tbl, code_pos_of_instr = assem_of_ir ir in
  let read_instr_of_chan = Ir.Chan.U.Table.create () in
  Set.iter user_readable_ports ~f:(fun port ->
      let chan = Hashtbl.find_exn chan_tbl port in
      let var_id = Assem_builder.new_assem_var_id ab in
      let read_instr = Assem_builder.push ab (Read (var_id, chan)) in
      let dequeuer = Read_dequeuer.create ~var_id ~equals:port.d.dtype.equal in
      let _ = Assem_builder.push ab (Read_dequeuer dequeuer) in
      Hashtbl.set read_instr_of_chan ~key:port ~data:read_instr);
  let send_instr_of_chan = Ir.Chan.U.Table.create () in
  Set.iter user_sendable_ports ~f:(fun port ->
      let chan = Hashtbl.find_exn chan_tbl port in
      let var_id = Assem_builder.new_assem_var_id ab in
      let send_instr = Assem_builder.push ab (Send (Var var_id, chan)) in
      let enqueuer = Send_enqueuer.create ~var_id in
      let _ = Assem_builder.push ab (Send_enqueuer enqueuer) in
      Hashtbl.set send_instr_of_chan ~key:port ~data:send_instr);
  let var_of_assem_id = Assem_builder.var_of_assem_id ab in
  let var_inits =
    Hashtbl.map var_of_assem_id ~f:(fun var_id ->
        Option.map var_id.d.init ~f:Any.of_magic)
  in
  let var_ct = Assem_builder.assem_var_ct ab + 1 in
  let sim = ASim.create (Assem_builder.assem_array ab) ~var_ct ~var_inits in
  {
    sim;
    send_instr_of_chan;
    read_instr_of_chan;
    to_send_at_next_wait = Queue.create ();
    to_read_at_next_wait = Queue.create ();
    is_done = false;
    var_of_assem_id = Assem_builder.var_of_assem_id ab;
    code_pos_of_instr;
  }

let to_ordered_alist_map queue =
  Queue.to_list queue |> Ir.Chan.U.Map.of_alist_multi

let wait t ?(max_steps = 1000) ?(line_numbers = true) () =
  let%bind.Result () =
    if t.is_done then Error (Error.of_string "Already errored") else Ok ()
  in
  let to_send = to_ordered_alist_map t.to_send_at_next_wait in
  let to_read = to_ordered_alist_map t.to_read_at_next_wait in
  Queue.clear t.to_send_at_next_wait;
  Queue.clear t.to_read_at_next_wait;
  let user_sends =
    Map.to_alist to_send
    |> List.map ~f:(fun (chan_id, values_with_pos) ->
           let values = List.map values_with_pos ~f:fst in
           assert (List.length values > 0);
           let send_instr = Hashtbl.find_exn t.send_instr_of_chan chan_id in
           (send_instr, values))
    |> Instr_idx.Map.of_alist_exn
  in
  let user_reads =
    Map.to_alist to_read
    |> List.map ~f:(fun (chan_id, values_with_pos) ->
           let values = List.map values_with_pos ~f:fst in
           assert (List.length values > 0);
           let read_instr = Hashtbl.find_exn t.read_instr_of_chan chan_id in
           (read_instr, values))
    |> Instr_idx.Map.of_alist_exn
  in
  let decode_instr instr_idx = Hashtbl.find_exn t.code_pos_of_instr instr_idx in
  let decode_var_id assem_id = Hashtbl.find_exn t.var_of_assem_id assem_id in
  let status = ASim.wait ~max_steps t.sim ~user_sends ~user_reads in
  let _line_numbers = line_numbers in
  (* TODO *)
  let decode_error err =
    let s_of_cp (cp : Code_pos.t) =
      sprintf "in %s on line %d" cp.filename cp.line_number
    in
    let s_of_var var_id =
      (decode_var_id var_id).d.creation_code_pos |> s_of_cp
    in
    let s_of_chan (chan_id : Ir.Chan.U.t) =
      chan_id.d.creation_code_pos |> s_of_cp
    in
    let s_of_instr instr = decode_instr instr |> s_of_cp in
    match err with
    | ASim.Wait_error.Already_errored -> assert false
    | Time_out -> "Simulation timed out. Maybe increase max_steps?"
    | Assert_failure inst_idx ->
        if line_numbers then
          sprintf "Assertion failed: %s." (s_of_instr inst_idx)
        else "Assertion failed."
    | Uninit_id (var_id, inst_idx) ->
        if line_numbers then
          sprintf "Uninitialized variable: read %s, created %s."
            (s_of_instr inst_idx) (s_of_var var_id)
        else "Uninitialized variable."
    | Simul_chan_senders (instr_1, instr_2) ->
        if line_numbers then
          sprintf
            "Simulatnious senders on channel: statement 1 %s, statement 2 %s."
            (s_of_instr instr_1) (s_of_instr instr_2)
        else "Simulatnious senders on channel."
    | Simul_chan_readers (instr_1, instr_2) ->
        if line_numbers then
          sprintf
            "Simulatnious readers on channel: statement 1 %s, statement 2 %s."
            (s_of_instr instr_1) (s_of_instr instr_2)
        else "Simulatnious readers on channel."
    | Simul_read_write (var_id, instr_1, instr_2) ->
        if line_numbers then
          sprintf
            "Simulatnious read and write of variable: statement 1 %s, \
             statement 2 %s, create %s."
            (s_of_instr instr_1) (s_of_instr instr_2) (s_of_var var_id)
        else "Simulatnious read and write of variable."
    | Simul_write_write (var_id, instr_1, instr_2) ->
        if line_numbers then
          sprintf
            "Simulatnious writes of variable: statement 1 %s, statement 2 %s, \
             create %s."
            (s_of_instr instr_1) (s_of_instr instr_2) (s_of_var var_id)
        else "Simulatnious writes of variable."
    | Select_no_guards_true inst_idx ->
        if line_numbers then
          sprintf "Select statement has no true guards: %s."
            (s_of_instr inst_idx)
        else "Select statement has no true guards."
    | Select_multiple_guards_true (true_idxs, inst_idx) ->
        if line_numbers then
          sprintf
            "Select statement has multiple true guards: %s, true branch \
             indices as %s."
            (s_of_instr inst_idx)
            ([%sexp (true_idxs : int list)] |> Sexp.to_string)
        else "Select statement has multiple true guards."
    | Read_dequeuer_wrong_value (dequeuer_instr, actual, expected, op_idx) ->
        let chan_id = chan_of_read_instr t dequeuer_instr in
        if line_numbers then
          let code_pos : Code_pos.t =
            Map.find_exn to_read chan_id
            |> (fun l -> List.nth_exn l op_idx)
            |> snd
          in
          sprintf
            "User read has wrong value: got %s, but expected %s based on \
             `send' function call %s, on chan created %s."
            (chan_id.d.dtype.sexp_of_t actual |> Sexp.to_string)
            (chan_id.d.dtype.sexp_of_t expected |> Sexp.to_string)
            (s_of_cp code_pos) (s_of_chan chan_id)
        else
          sprintf "User read has wrong value: got %s, but expected %s"
            (chan_id.d.dtype.sexp_of_t actual |> Sexp.to_string)
            (chan_id.d.dtype.sexp_of_t expected |> Sexp.to_string)
    | Read_dequeuer_not_done (dequeuer_instr, op_idx) ->
        if line_numbers then
          let chan_id = chan_of_read_instr t dequeuer_instr in
          let code_pos : Code_pos.t =
            Map.find_exn to_read chan_id
            |> (fun l -> List.nth_exn l op_idx)
            |> snd
          in
          sprintf "User read did not complete:  called %s, on chan created %s."
            (s_of_cp code_pos) (s_of_chan chan_id)
        else sprintf "User read did not complete."
    | Send_enqueuer_not_done (enqueuer_instr, op_idx) ->
        if line_numbers then
          let chan_id = chan_of_send_instr t enqueuer_instr in
          let code_pos : Code_pos.t =
            Map.find_exn to_send chan_id
            |> (fun l -> List.nth_exn l (op_idx - 1))
            |> snd
          in
          sprintf "User send did not complete:  called %s, on chan created %s."
            (s_of_cp code_pos) (s_of_chan chan_id)
        else sprintf "User send did not complete."
    | Mem_out_of_bounds (mem_instr, idx, len) ->
        if line_numbers then
          sprintf "Mem access out of bounds: %s, idx is %d, size of mem is %d."
            (s_of_instr mem_instr) idx len
        else
          sprintf "Mem access out of bounds: idx is %d, size of mem is %d." idx
            len
  in
  let status = Result.map_error status ~f:decode_error in
  let status = Result.map_error status ~f:Error.of_string in
  Result.iter_error status ~f:(fun _ -> t.is_done <- true);
  status

let wait' t ?max_steps () =
  print_s [%sexp (wait t ?max_steps () : unit Or_error.t)]

let send t ?loc chan_id value =
  let chan_id = Ir.Chan.unwrap_w chan_id in
  let call_site = Code_pos.value_or_psite loc in
  match Hashtbl.mem t.send_instr_of_chan chan_id with
  | true ->
      Queue.enqueue t.to_send_at_next_wait
        (chan_id, (Any.of_magic value, call_site))
  | false ->
      failwith
        "the provided chan_id was not regestered as a user-sendable chan in \
         Sim.create"

let read t ?loc chan_id value =
  let chan_id = Ir.Chan.unwrap_r chan_id in
  let call_site = Code_pos.value_or_psite loc in
  match Hashtbl.mem t.read_instr_of_chan chan_id with
  | true ->
      Queue.enqueue t.to_read_at_next_wait
        (chan_id, (Any.of_magic value, call_site))
  | false ->
      failwith
        "the provided chan_id was not regestered as a user-readable chan in \
         Sim.create"
