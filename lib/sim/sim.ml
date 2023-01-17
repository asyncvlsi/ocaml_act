open! Core
open! Act
module Ir = Internal_rep

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
let to_unit_result o ~f = match o with Some v -> Error (f v) | None -> Ok ()

module With_origin = struct
  type 'a t = { value : 'a; origin : Code_pos.t } [@@deriving sexp_of]

  let map { value; origin } ~f = { value = f value; origin }
end

module Instr_idx = struct
  include Int

  let dummy_val = -1
  let next i = i + 1
end

module Var_id = Int
module Chan_id = Int
module Mem_id = Int

module Expr = struct
  type t =
    | Var of Var_id.t
    | Const of Any.t
    | Map of t * (Any.t -> Any.t)
    | Map2 of t * t * (Any.t -> Any.t -> Any.t)
    | AssertMap2 of
        t * t * (Any.t -> Any.t -> string option) * (Any.t -> Any.t -> Any.t)
  [@@deriving sexp_of]

  let map e ~f = Map (e, Obj.magic f)
  let map2 e1 e2 ~f = Map2 (e1, e2, Obj.magic f)

  let assert_map2 e1 e2 ~assert_fn ~f =
    AssertMap2 (e1, e2, Obj.magic assert_fn, Obj.magic f)

  let rec var_ids t =
    match t with
    | Var id -> [ id ]
    | Const _ -> []
    | Map (e, _) -> var_ids e
    | Map2 (e1, e2, _) -> var_ids e1 @ var_ids e2
    | AssertMap2 (e1, e2, _, _) -> var_ids e1 @ var_ids e2
end

module Var_id_src = struct
  type t = Var of Ir.Var.U.t | Mem_idx_reg | Read_deq_reg | Send_enq_reg
end

module Var_buff = struct
  type t = {
    (* immutable data *)
    src : Var_id_src.t;
    dtype : Any.t Ir.DType.t;
    (* mutable *)
    mutable value : Any.t;
    mutable is_inited : bool;
    mutable read_ct : int;
    mutable write_ct : int;
  }
end

module Chan_buff = struct
  type t = {
    (* immutable data *)
    src : (Ir.Chan.U.t[@sexp.opaque]);
    (* mutable *)
    mutable read_ready : bool;
    mutable read_instr : Instr_idx.t;
    mutable read_dst_var_id : Var_id.t;
    mutable send_ready : bool;
    mutable send_instr : Instr_idx.t;
    mutable send_expr : Expr.t;
    waiting_on_send_ready : Instr_idx.t Vec.t;
    waiting_on_read_ready : Instr_idx.t Vec.t;
  }
  [@@deriving sexp_of]

  let create src =
    {
      src;
      read_ready = false;
      send_ready = false;
      read_instr = Instr_idx.dummy_val;
      send_instr = Instr_idx.dummy_val;
      read_dst_var_id = 0;
      send_expr = Const (Obj.magic 0);
      waiting_on_send_ready = Vec.create ~cap:10 ~default:0;
      waiting_on_read_ready = Vec.create ~cap:10 ~default:0;
    }
end

module Mem_buff = struct
  type t = {
    (* immutable data *)
    src : (Ir.Mem.t[@sexp.opaque]);
    (* mutable *)
    arr : Any.t array;
    idx_helper_reg : Var_id.t;
  }
  [@@deriving sexp_of]

  let create ~idx_helper_reg src =
    { src; arr = Array.copy src.d.init; idx_helper_reg }
end

module Send_enqueuer = struct
  type t = {
    mutable to_send : Any.t With_origin.t array;
    var_id : Var_id.t;
    mutable is_done : bool;
    mutable idx : int;
    chan : Chan_id.t;
  }
  [@@deriving sexp_of]

  let create ~var_id chan =
    { to_send = [||]; var_id; is_done = true; idx = 0; chan }
end

module Read_dequeuer = struct
  type t = {
    mutable expected_reads : Any.t With_origin.t array;
    var_id : Var_id.t;
    equals : (Any.t -> Any.t -> bool[@sexp.opaque]);
    mutable idx : int;
    chan : Chan_id.t;
  }
  [@@deriving sexp_of]

  let create ~var_id ~equals chan =
    { expected_reads = [||]; var_id; equals; idx = 0; chan }
end

module Par_join = struct
  type t = { max_ct : int; mutable curr_ct : int } [@@deriving sexp]

  let create ~max_ct = { max_ct; curr_ct = 0 }
end

module N = struct
  type t =
    | End
    | Nop
    | Assign of Var_id.t * Expr.t
    | Log0 of string
    | Log1 of Expr.t * (Any.t -> string)
    | Assert of Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
    | SelectImm of (Expr.t * Instr_idx.t) list
    | SelectImmElse of (Expr.t * Instr_idx.t) list * Instr_idx.t
    | Read of Var_id.t * Chan_id.t
    | Send of Expr.t * Chan_id.t
    | ReadMem of
        (* idx *) Expr.t * (* dst *) Var_id.t * (* reg *) Var_id.t * Mem_id.t
    | WriteMem of
        (* idx *) Expr.t * (* src *) Expr.t * (* idx_reg *) Var_id.t * Mem_id.t
    | WaitUntilReadReady of Chan_id.t
    | WaitUntilSendReady of Chan_id.t
    (* These allow for probabilistic testing that a probe is stable *)
    | AssertStillReadReady of Chan_id.t
    | AssertStillSendReady of Chan_id.t
    (* These are ``magic'' instructions that allow user io operations. These instruction
       should be placed immediatly after the assoiated send/read instruction *)
    | Send_enqueuer of Send_enqueuer.t
    | Read_dequeuer of Read_dequeuer.t
  [@@deriving sexp_of]

  let read_ids t =
    (match t with
    | End | Nop | Par _ | ParJoin _ -> []
    | Log0 _ | Read (_, _) | Jump _ -> []
    | Log1 (expr, _) -> Expr.var_ids expr
    | Assign (_, expr) -> Expr.var_ids expr
    | Assert expr | JumpIfFalse (expr, _) -> Expr.var_ids expr
    | SelectImm l | SelectImmElse (l, _) ->
        List.concat_map l ~f:(fun (expr, _) -> Expr.var_ids expr)
    | Send (expr, _) -> Expr.var_ids expr
    | Send_enqueuer _ -> []
    | Read_dequeuer _ ->
        (* intentionally none, since this shouldnt be threaded *) []
    | ReadMem (idx_expr, _, _, _) -> Expr.var_ids idx_expr
    | WriteMem (idx_expr, src_expr, _, _) ->
        Expr.var_ids idx_expr @ Expr.var_ids src_expr
    | WaitUntilSendReady _ | WaitUntilReadReady _ -> []
    | AssertStillReadReady _ | AssertStillSendReady _ -> [])
    |> Var_id.Set.of_list

  let write_ids t =
    (match t with
    | End | Nop | Par _ | ParJoin _ -> []
    | Log0 _ | Log1 _ | Assert _ | JumpIfFalse (_, _) | Jump _ -> []
    | SelectImm _ | SelectImmElse _ | Send (_, _) -> []
    | Assign (id, _) -> [ id ]
    | Read (var_id, _) -> [ var_id ]
    | Send_enqueuer _ ->
        (* intentionally none, since this shouldnt be threaded *) []
    | Read_dequeuer _ -> []
    | ReadMem (_, dst, mem_idx_reg, _) -> [ dst; mem_idx_reg ]
    | WriteMem (_, _, mem_idx_reg, _) -> [ mem_idx_reg ]
    | WaitUntilSendReady _ | WaitUntilReadReady _ -> []
    | AssertStillReadReady _ | AssertStillSendReady _ -> [])
    |> Var_id.Set.of_list
end

module Queued_user_op = struct
  type t = {
    queuer : [ `Send of Send_enqueuer.t | `Read of Read_dequeuer.t ];
    chan_instr : Instr_idx.t;
    value : Any.t;
    call_site : Code_pos.t;
  }
end

type t = {
  assem : N.t array;
  loc_of_assem_idx : Code_pos.t array;
  all_enqueuers : (Instr_idx.t * Send_enqueuer.t) Ir.Chan.U.Map.t;
  all_dequeuers : (Instr_idx.t * Read_dequeuer.t) Ir.Chan.U.Map.t;
  (* per-wait state *)
  queued_user_ops : Queued_user_op.t Queue.t;
  (* simulation state *)
  pcs : Instr_idx.t Vec.t;
  var_table : Var_buff.t array;
  chan_table : Chan_buff.t array;
  mem_table : Mem_buff.t array;
  rng : (Random.State.t[@sexp.opaque]);
  mutable is_done : bool;
}

let check_value_fits_in_dtype dtype ~value ~error =
  match Ir.DType.fits_value dtype value with
  | true -> Ok ()
  | false -> Error error

let step' t ~pc_idx =
  let eval_var_table expr ~(on_error : string -> 'b) : (Any.t, 'b) result =
    let rec eval expr =
      match expr with
      | Expr.Var id -> Ok t.var_table.(id).value
      | Const c -> Ok c
      | Map (e, f) ->
          let%map.Result e = eval e in
          f e
      | Map2 (e1, e2, f) ->
          let%bind.Result e1 = eval e1 in
          let%map.Result e2 = eval e2 in
          f e1 e2
      | AssertMap2 (e1, e2, assert_fn, f) -> (
          let%bind.Result e1 = eval e1 in
          let%bind.Result e2 = eval e2 in
          match assert_fn e1 e2 with
          | None -> Ok (f e1 e2)
          | Some raw_error -> Error (on_error raw_error))
    in
    eval expr
  in
  let eval_bool expr ~on_error =
    eval_var_table expr ~on_error |> Result.map ~f:Any.to_bool
  in
  let eval_int expr ~on_error =
    eval_var_table expr ~on_error |> Result.map ~f:Any.to_int
  in
  let set_var_table ~var_id ~value =
    let t = t.var_table in
    t.(var_id).value <- value;
    t.(var_id).is_inited <- true
  in
  let at_var_table ~var_id =
    let t = t.var_table in
    assert t.(var_id).is_inited;
    t.(var_id).value
  in

  let guard pc =
    let read_ids = N.read_ids t.assem.(pc) in
    let write_ids = N.write_ids t.assem.(pc) in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id -> t.var_table.(read_id).write_ct > 0)
      |> to_unit_result ~f:(fun read_id -> `Reading_written_var (read_id, pc))
    in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id -> not t.var_table.(read_id).is_inited)
      |> to_unit_result ~f:(fun read_id -> `Uninit_id (read_id, pc))
    in
    let%bind.Result () =
      Set.find write_ids ~f:(fun write_id -> t.var_table.(write_id).read_ct > 0)
      |> to_unit_result ~f:(fun write_id -> `Writing_read_var (write_id, pc))
    in
    let%map.Result () =
      Set.find write_ids ~f:(fun write_id ->
          t.var_table.(write_id).write_ct > 0)
      |> to_unit_result ~f:(fun write_id -> `Writing_written_var (write_id, pc))
    in
    Set.iter read_ids ~f:(fun read_id ->
        t.var_table.(read_id).read_ct <- t.var_table.(read_id).read_ct + 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.var_table.(write_id).write_ct <- t.var_table.(write_id).write_ct + 1)
  in
  let set_pc_and_guard ~pc_idx new_pc =
    Vec.set t.pcs pc_idx new_pc |> fun () -> guard new_pc
  in
  let push_pc_and_guard new_pc =
    Vec.push t.pcs new_pc |> fun () -> guard new_pc
  in
  let unguard pc =
    let read_ids = N.read_ids t.assem.(pc) in
    let write_ids = N.write_ids t.assem.(pc) in
    Set.iter read_ids ~f:(fun read_id ->
        t.var_table.(read_id).read_ct <- t.var_table.(read_id).read_ct - 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.var_table.(write_id).write_ct <- t.var_table.(write_id).write_ct - 1)
  in

  let step_chan (chan : Chan_buff.t) =
    if chan.read_ready && chan.send_ready then (
      chan.read_ready <- false;
      chan.send_ready <- false;
      unguard chan.read_instr;
      unguard chan.send_instr;
      let%bind.Result value =
        eval_var_table chan.send_expr ~on_error:(fun error_string ->
            `Eval_send_expr_failed (chan.send_instr, error_string))
      in
      let%bind.Result () =
        check_value_fits_in_dtype chan.src.d.dtype ~value
          ~error:
            (`Sent_value_doesnt_fit_in_chan
              ( chan.send_instr,
                chan,
                Ir.DType.layout chan.src.d.dtype,
                Ir.DType.sexp_of_t_ chan.src.d.dtype value ))
      in
      let%bind.Result () =
        let var_dtype = t.var_table.(chan.read_dst_var_id).dtype in
        check_value_fits_in_dtype var_dtype ~value
          ~error:
            (`Read_chan_value_doesnt_fit_in_var
              ( chan.read_instr,
                chan.read_dst_var_id,
                Ir.DType.layout var_dtype,
                Ir.DType.sexp_of_t_ var_dtype value ))
      in
      let () = set_var_table ~var_id:chan.read_dst_var_id ~value in
      let () = Vec.extend t.pcs [ chan.read_instr + 1; chan.send_instr + 1 ] in
      let%bind.Result () = guard (chan.read_instr + 1) in
      guard (chan.send_instr + 1))
    else Ok ()
  in

  let step_select l ~else_ ~pc ~pc_idx =
    let%bind.Result branches =
      List.mapi l ~f:(fun idx (expr, instr) ->
          let%map.Result guard =
            eval_bool expr ~on_error:(fun error_string ->
                `Eval_guard_expr_failed (pc, idx, error_string))
          in
          (guard, instr, idx))
      |> Result.all
    in
    let true_branches =
      List.filter branches ~f:(fun (g, _, _) -> g)
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
      let%bind.Result value =
        eval_var_table expr ~on_error:(fun error_string ->
            `Eval_assign_expr_failed (pc, error_string))
      in
      let%bind.Result () =
        let var_dtype = t.var_table.(var_id).dtype in
        check_value_fits_in_dtype var_dtype ~value
          ~error:
            (`Assigned_value_doesnt_fit_in_var
              ( pc,
                var_id,
                Ir.DType.layout var_dtype,
                Ir.DType.sexp_of_t_ var_dtype value ))
      in
      let () = set_var_table ~var_id ~value in
      set_pc_and_guard ~pc_idx (pc + 1)
  | Assert expr -> (
      unguard pc;
      let%bind.Result expr =
        eval_bool expr ~on_error:(fun error_string ->
            `Eval_assert_expr_failed (pc, error_string))
      in
      match expr with
      | true -> set_pc_and_guard ~pc_idx (pc + 1)
      | false -> Error (`Assert_failure pc))
  | Log0 str ->
      (* unguard pc; *)
      printf "%s" str;
      set_pc_and_guard ~pc_idx (pc + 1)
  | Log1 (expr, f) ->
      unguard pc;
      let%bind.Result expr =
        eval_var_table expr ~on_error:(fun error_string ->
            `Eval_log1_expr_failed (pc, error_string))
      in
      printf "%s" (f expr);
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
      let%bind.Result expr =
        eval_bool expr ~on_error:(fun error_string ->
            `Eval_Jump_if_false_expr_failed (pc, error_string))
      in
      set_pc_and_guard ~pc_idx (if expr then pc + 1 else inst)
  | SelectImm l ->
      unguard pc;
      step_select l ~else_:None ~pc ~pc_idx
  | SelectImmElse (l, else_) ->
      unguard pc;
      step_select l ~else_:(Some else_) ~pc ~pc_idx
  | Read (dst_id, chan_idx) ->
      (* unguard pc; *)
      let chan = t.chan_table.(chan_idx) in
      if chan.read_ready then Error (`Simul_chan_readers (chan.read_instr, pc))
      else (
        chan.read_ready <- true;
        chan.read_instr <- pc;
        chan.read_dst_var_id <- dst_id;
        Vec.remove t.pcs pc_idx;
        Vec.iter chan.waiting_on_read_ready ~f:(fun waiting_pc ->
            (* It doesnt need gaurding becaus the waiting_pc is required to be a WaitUntilReadable
               or WaitUntilSendable node, which has no read/written variables *)
            Vec.push t.pcs waiting_pc);
        Vec.clear chan.waiting_on_read_ready;
        step_chan chan)
  | Send (expr, chan_idx) ->
      (* unguard pc; *)
      let chan = t.chan_table.(chan_idx) in
      if chan.send_ready then Error (`Simul_chan_senders (chan.send_instr, pc))
      else (
        chan.send_ready <- true;
        chan.send_instr <- pc;
        chan.send_expr <- expr;
        Vec.remove t.pcs pc_idx;
        Vec.iter chan.waiting_on_send_ready ~f:(fun waiting_pc ->
            (* It doesnt need gaurding becaus the waiting_pc is required to be a WaitUntilReadable
               or WaitUntilSendable node, which has no read/written variables *)
            Vec.push t.pcs waiting_pc);
        Vec.clear chan.waiting_on_send_ready;
        step_chan chan)
  | WaitUntilSendReady chan_idx ->
      (* unguard pc; *)
      let chan = t.chan_table.(chan_idx) in
      if chan.send_ready then set_pc_and_guard ~pc_idx (pc + 1)
      else (
        Vec.remove t.pcs pc_idx;
        Vec.push chan.waiting_on_send_ready (pc + 1);
        Ok ())
  | WaitUntilReadReady chan_idx ->
      (* unguard pc; *)
      let chan = t.chan_table.(chan_idx) in
      if chan.read_ready then set_pc_and_guard ~pc_idx (pc + 1)
      else (
        Vec.remove t.pcs pc_idx;
        Vec.push chan.waiting_on_read_ready (pc + 1);
        Ok ())
  | AssertStillSendReady chan_idx ->
      let chan = t.chan_table.(chan_idx) in
      if chan.read_ready then set_pc_and_guard ~pc_idx (pc + 1)
      else Error (`Unstable_wait_until_send_ready (pc, chan_idx))
  | AssertStillReadReady chan_idx ->
      let chan = t.chan_table.(chan_idx) in
      if chan.read_ready then set_pc_and_guard ~pc_idx (pc + 1)
      else Error (`Unstable_wait_until_read_ready (pc, chan_idx))
  | Send_enqueuer d ->
      (* unguard pc; *)
      assert (not d.is_done);
      if d.idx >= Array.length d.to_send then (
        Vec.remove t.pcs pc_idx;
        d.is_done <- true;
        Ok ())
      else
        let { With_origin.value; origin = _ } = d.to_send.(d.idx) in
        d.idx <- d.idx + 1;
        set_var_table ~var_id:d.var_id ~value;
        set_pc_and_guard ~pc_idx (pc - 1)
  | Read_dequeuer deq ->
      unguard pc;
      let value = at_var_table ~var_id:deq.var_id in
      let expected = deq.expected_reads.(deq.idx) in
      if not (deq.equals value expected.value) then
        let chan_dtype = t.chan_table.(deq.chan).src.d.dtype in
        let value = Ir.DType.sexp_of_t_ chan_dtype value in
        let expected =
          With_origin.map expected ~f:(fun expected ->
              Ir.DType.sexp_of_t_ chan_dtype expected)
        in
        Error
          (`Read_dequeuer_wrong_value
            (pc, t.chan_table.(deq.chan).src, value, expected, deq.idx))
      else (
        deq.idx <- deq.idx + 1;
        if deq.idx >= Array.length deq.expected_reads then (
          Vec.remove t.pcs pc_idx;
          Ok ())
        else set_pc_and_guard ~pc_idx (pc - 1))
  | ReadMem (idx_expr, dst_id, _, mem_id) ->
      unguard pc;
      let mem = t.mem_table.(mem_id) in
      let%bind.Result idx =
        eval_int idx_expr ~on_error:(fun error_string ->
            `Eval_mem_idx_expr_failed (pc, error_string))
      in
      if idx < 0 || idx >= Array.length mem.arr then
        Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
      else
        let value = mem.arr.(idx) in
        let%bind.Result () =
          let var_dtype = t.var_table.(dst_id).dtype in
          check_value_fits_in_dtype var_dtype ~value
            ~error:
              (`Read_mem_value_doesnt_fit_in_var
                ( pc,
                  mem,
                  dst_id,
                  Ir.DType.layout var_dtype,
                  Ir.DType.sexp_of_t_ var_dtype value ))
        in
        let () = set_var_table ~var_id:dst_id ~value:mem.arr.(idx) in
        set_pc_and_guard ~pc_idx (pc + 1)
  | WriteMem (idx_expr, src_expr, _, mem_id) ->
      unguard pc;
      let mem = t.mem_table.(mem_id) in
      let%bind.Result idx =
        eval_int idx_expr ~on_error:(fun error_string ->
            `Eval_mem_idx_expr_failed (pc, error_string))
      in
      if idx < 0 || idx >= Array.length mem.arr then
        Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
      else
        let%bind.Result value =
          eval_var_table src_expr ~on_error:(fun error_string ->
              `Eval_write_mem_value_expr_failed (pc, error_string))
        in
        let%bind.Result () =
          check_value_fits_in_dtype mem.src.d.dtype ~value
            ~error:
              (`Written_mem_value_doesnt_fit_in_cell
                ( pc,
                  mem,
                  Ir.DType.layout mem.src.d.dtype,
                  Ir.DType.sexp_of_t_ mem.src.d.dtype value ))
        in
        let () = mem.arr.(idx) <- value in
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
  Array.find_map t.chan_table ~f:rw_id_of_chan
  |> some_or_thunk ~f:(fun () -> Vec.find t.pcs ~f:is_rw)
  |> Option.value_exn

let resolve_step_err t e ~line_numbers =
  let find_reader var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.read_ids
  in
  let find_writer var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:N.write_ids
  in
  let loc_of_instr var_id = t.loc_of_assem_idx.(var_id) in
  let str_l (cp : Code_pos.t) =
    if line_numbers then
      [%string "in %{cp.filename} on line %{cp.line_number#Int}"]
    else "<loc>"
  in
  let str_i pc = loc_of_instr pc |> str_l in
  let simul_read_write_var var_id ~read_pc ~write_pc =
    let var =
      match t.var_table.(var_id).src with
      | Var var -> var
      | Mem_idx_reg | Read_deq_reg | Send_enq_reg ->
          failwith "should be unreachable"
    in
    let var_decl = var.d.creation_code_pos in
    [%string
      "Simultanious read and write of variable: read %{str_i read_pc}, write \
       %{str_i write_pc}, create %{str_l var_decl}."]
  in
  match e with
  | `Stuck ->
      (* check whether we need error because the queuers are unfinished *)
      let%bind.Result () =
        Map.data t.all_dequeuers
        |> List.find ~f:(fun (_, d) -> d.idx < Array.length d.expected_reads)
        |> to_unit_result ~f:(fun (_, deq) ->
               [%string
                 "User read did not complete:  called %{str_l \
                  deq.expected_reads.(deq.idx).origin}, on chan created \
                  %{str_l t.chan_table.(deq.chan).src.d.creation_code_pos}."])
      in
      let%bind.Result () =
        Map.data t.all_enqueuers
        |> List.find ~f:(fun (_, d) -> not d.is_done)
        |> to_unit_result ~f:(fun (_, enq) ->
               [%string
                 "User send did not complete:  called %{str_l \
                  enq.to_send.(enq.idx - 1).origin}, on chan created %{str_l \
                  t.chan_table.(enq.chan).src.d.creation_code_pos}."])
      in
      Ok `Stuck
  | `Uninit_id (var_id, pc) ->
      let var_code_pos =
        match t.var_table.(var_id).src with
        | Var var -> str_l var.d.creation_code_pos
        | Mem_idx_reg | Read_deq_reg | Send_enq_reg -> failwith "unreachable"
      in
      Error
        [%string
          "Uninitialized variable: read %{str_i pc}, created %{var_code_pos}."]
  | `Simul_chan_senders (fst_pc, snd_pc) ->
      Error
        [%string
          "Simultanious senders on channel: statement 1 %{str_i fst_pc}, \
           statement 2 %{str_i snd_pc}."]
  | `Simul_chan_readers (fst_pc, snd_pc) ->
      Error
        [%string
          "Simultanious readers on channel: statement 1 %{str_i fst_pc}, \
           statement 2 %{str_i snd_pc}."]
  | `Assert_failure pc -> Error [%string "Assertion failed: %{str_i pc}."]
  | `Reading_written_var (var_id, pc) ->
      Error
        (simul_read_write_var var_id ~read_pc:pc
           ~write_pc:(find_writer var_id ~ignore:pc))
  | `Writing_read_var (var_id, pc) ->
      Error
        (simul_read_write_var var_id
           ~read_pc:(find_reader var_id ~ignore:pc)
           ~write_pc:pc)
  | `Writing_written_var (var_id, pc) -> (
      (* This could either be because an actual variable is written in two locations,
         or could be the fake variable used by a memory to mark that two accesses are
         happening at the same time *)
      let pc2 = find_writer var_id ~ignore:pc in
      match t.var_table.(var_id).src with
      | Var var ->
          Error
            [%string
              "Simulatnious writes of variable: statement 1 %{str_i pc}, \
               statement 2 %{str_i pc2}, create %{str_l \
               var.d.creation_code_pos}."]
      | Mem_idx_reg ->
          Error
            [%string
              "Simulatnious accesses of a memory/rom: statement 1 %{str_i pc}, \
               statement 2 %{str_i pc2}."]
      | Read_deq_reg | Send_enq_reg -> failwith "unreachable")
  | `Select_no_guards_true pc ->
      Error [%string "Select statement has no true guards: %{str_i pc}."]
  | `Select_multiple_guards_true (pc, branch_idxs) ->
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true guards: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]
  | `Read_dequeuer_wrong_value (_, ir_chan, actual, expected, _) ->
      let chan_decl = ir_chan.Ir.Chan.U.d.creation_code_pos in
      Error
        [%string
          "User read has wrong value: got %{actual#Sexp}, but expected \
           %{expected.With_origin.value#Sexp} based on `send' function call \
           %{str_l expected.origin}, on chan created %{str_l chan_decl}."]
  | `Mem_out_of_bounds (pc, idx, len) ->
      Error
        [%string
          "Mem access out of bounds: %{str_i pc}, idx is %{idx#Int}, size of \
           mem is %{len#Int}."]
  | `Unstable_wait_until_read_ready (pc, _) ->
      Error [%string "Unstable wait_until_read_ready: %{str_i pc}."]
  | `Unstable_wait_until_send_ready (pc, _) ->
      Error [%string "Unstable wait_until_send_ready: %{str_i pc}."]
  | `Assigned_value_doesnt_fit_in_var (assign_instr, _, var_layout, value) ->
      Error
        [%string
          "Assigned value doesnt fit in var: got %{value#Sexp} but variable \
           has layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           assign_instr}."]
  | `Read_chan_value_doesnt_fit_in_var (read_instr, _, var_layout, value) ->
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           read_instr}."]
  | `Read_mem_value_doesnt_fit_in_var (read_instr, _, _, var_layout, value) ->
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           read_instr}."]
  | `Sent_value_doesnt_fit_in_chan (send_instr, _, chan_layout, value) ->
      Error
        [%string
          "Sent value doesnt fit in chan: got %{value#Sexp} but channel has \
           layout %{Ir.Layout.sexp_of_t chan_layout#Sexp} at %{str_i \
           send_instr}."]
  | `Written_mem_value_doesnt_fit_in_cell
      (write_instr, _, mem_cell_layout, value) ->
      Error
        [%string
          "Written value doesnt fit in memory cell: got %{value#Sexp} but \
           memory cell has layout %{Ir.Layout.sexp_of_t mem_cell_layout#Sexp} \
           at %{str_i write_instr}."]
  | `Eval_Jump_if_false_expr_failed _ ->
      failwith "TODO - Eval expr Eval_Jump_if_false_expr_failed"
  | `Eval_assert_expr_failed _ ->
      failwith "TODO - Eval expr Eval_assert_expr_failed"
  | `Eval_assign_expr_failed _ ->
      failwith "TODO - Eval expr Eval_assign_expr_failed"
  | `Eval_guard_expr_failed _ ->
      failwith "TODO - Eval expr Eval_guard_expr_failed"
  | `Eval_mem_idx_expr_failed _ ->
      failwith "TODO - Eval expr Eval_mem_idx_expr_failed"
  | `Eval_send_expr_failed _ ->
      failwith "TODO - Eval expr Eval_send_expr_failed"
  | `Eval_write_mem_value_expr_failed _ ->
      failwith "TODO - Eval expr Eval_write_mem_value_expr_failed"
  | `Eval_log1_expr_failed _ ->
      failwith "TODO - Eval expr Eval_log1_expr_failed"

let schedual_user_chan_ops t ~user_sends ~user_reads =
  let set_user_sends t ~values ~send_instr =
    let enqueuer =
      match (t.assem.(send_instr), t.assem.(send_instr + 1)) with
      | Send _, Send_enqueuer enqueuer -> enqueuer
      | _ -> assert false
    in
    enqueuer.is_done <- false;
    enqueuer.idx <- 0;
    enqueuer.to_send <- Array.of_list values;
    Vec.push t.pcs (send_instr + 1)
  in
  let set_user_reads t ~values ~read_instr =
    let dequeuer =
      match (t.assem.(read_instr), t.assem.(read_instr + 1)) with
      | Read _, Read_dequeuer dequeuer -> dequeuer
      | _ -> assert false
    in
    dequeuer.idx <- 0;
    dequeuer.expected_reads <- Array.of_list values;
    Vec.push t.pcs read_instr
  in
  Map.iteri user_sends ~f:(fun ~key:send_instr ~data:values ->
      set_user_sends t ~values ~send_instr);
  Map.iteri user_reads ~f:(fun ~key:read_instr ~data:values ->
      set_user_reads t ~values ~read_instr)

let wait t ?(max_steps = 1000) ?(line_numbers = true) () =
  let queued_user_ops = Queue.to_list t.queued_user_ops in
  Queue.clear t.queued_user_ops;

  let%bind.Result () =
    if t.is_done then Error (Error.of_string "Already errored") else Ok ()
  in
  let to_send, to_read =
    List.partition_map queued_user_ops
      ~f:(fun { queuer; chan_instr; value; call_site } ->
        match queuer with
        | `Send enqueuer -> First (chan_instr, (enqueuer, value, call_site))
        | `Read dequeuer -> Second (chan_instr, (dequeuer, value, call_site)))
  in
  let user_sends =
    Int.Map.of_alist_multi to_send
    |> Map.map ~f:(fun l ->
           let _enqueuer, _, _ = List.hd_exn l in
           let values_with_call_site =
             List.map l ~f:(fun (_, value, origin) ->
                 { With_origin.value; origin })
           in
           values_with_call_site)
  in
  let user_reads =
    Int.Map.of_alist_multi to_read
    |> Map.map ~f:(fun l ->
           let _enqueuer, _, _ = List.hd_exn l in
           let values_with_call_site =
             List.map l ~f:(fun (_, value, origin) ->
                 { With_origin.value; origin })
           in
           values_with_call_site)
  in
  let status =
    let%bind.Result () =
      match t.is_done with false -> Ok () | true -> Error "Already Errored"
    in
    let step_loop _ =
      match step t with
      | Ok () -> `Continue
      | Error e ->
          let status = resolve_step_err t e ~line_numbers in
          Result.iter_error status ~f:(fun _ -> t.is_done <- true);
          `Return status
    in
    schedual_user_chan_ops t ~user_sends ~user_reads;
    for_loop_else max_steps
      ~f:(fun step_idx -> step_loop step_idx)
      ~else_:(Error "Simulation timed out. Maybe increase max_steps?")
  in

  let status = Result.map_error status ~f:Error.of_string in
  Result.iter_error status ~f:(fun _ -> t.is_done <- true);
  Result.map status ~f:(fun `Stuck -> ())

let wait' t ?max_steps () =
  print_s [%sexp (wait t ?max_steps () : unit Or_error.t)]

module Assem_builder = struct
  type t = { assem : (N.t * Code_pos.t) Vec.t }

  let create () =
    { assem = Vec.create ~cap:10 ~default:(N.End, Code_pos.dummy_loc) }

  let push t loc (instr : N.t) =
    Vec.push t.assem (instr, loc);
    Vec.length t.assem - 1

  let edit t loc idx (instr : N.t) = Vec.set t.assem idx (instr, loc)
  let next_idx t = Vec.length t.assem
  let assem_array t = Vec.to_array t.assem
end

module Var_id_pool = struct
  type t = {
    mutable next_id : int;
    var_id_of_var : Var_id.t Ir.Var.U.Table.t;
    src_of_var_id : (Any.t Ir.DType.t * Var_id_src.t) Var_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      var_id_of_var = Ir.Var.U.Table.create ();
      src_of_var_id = Var_id.Table.create ();
    }

  let new_id t src dtype =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.set t.src_of_var_id ~key:id ~data:(dtype, src);
    id

  let to_assem_id t var =
    Hashtbl.find_or_add t.var_id_of_var var ~default:(fun () ->
        new_id t (Var_id_src.Var var) var.d.dtype)

  let to_assem_expr t expr =
    let rec convert : 'a. 'a Ir.Expr.t -> Expr.t =
      let imap2 a b f = Expr.map2 (convert a) (convert b) ~f in
      fun (type a) (x : a Ir.Expr.t) ->
        match x with
        | Ir.Expr.Var var_id -> Expr.Var (to_assem_id t var_id.u)
        | Const c -> Const (Any.of_magic (c : CInt.t))
        | Add (a, b) -> imap2 a b CInt.( + )
        | Sub (a, b) ->
            Expr.assert_map2 (convert a) (convert b)
              ~assert_fn:(fun a b ->
                if CInt.(a >= b) then None
                else
                  Some
                    [%string
                      "Expr.Sub must have first arg >= second arg but \
                       %{a#CInt} < %{a#CInt}"])
              ~f:CInt.( - )
        | Mul (a, b) -> imap2 a b CInt.( * )
        | Div (a, b) -> imap2 a b CInt.( / )
        | Mod (a, b) -> imap2 a b CInt.( % )
        | LShift (a, b) -> imap2 a b CInt.shift_left
        | LogicalRShift (a, b) -> imap2 a b CInt.shift_right_logical
        | BitAnd (a, b) -> imap2 a b CInt.bit_and
        | BitOr (a, b) -> imap2 a b CInt.bit_or
        | BitXor (a, b) -> imap2 a b CInt.bit_xor
        | Eq (a, b) -> imap2 a b CInt.equal
        | Ne (a, b) -> imap2 a b (fun a b -> not (CInt.equal a b))
        | Not a -> Expr.map (convert a) ~f:not
        | Magic_EnumToCInt (a, f) -> Expr.map (convert a) ~f
        | Magic_EnumOfCInt (a, f) -> Expr.map (convert a) ~f
        | Clip (a, bits) -> Expr.map (convert a) ~f:(CInt.clip ~bits)
        | Add_wrap (a, b, bits) -> imap2 a b (CInt.add_wrap ~bits)
        | Sub_wrap (a, b, bits) -> imap2 a b (CInt.sub_wrap ~bits)
    in
    convert expr

  let var_inits t = t.src_of_var_id
  (* let dtype_of_id t id = Hashtbl.find_exn t.dtype_of_var_id id *)
end

module Chan_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_chan : Chan_id.t Ir.Chan.U.Table.t;
    chan_of_id : Ir.Chan.U.t Chan_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      id_of_chan = Ir.Chan.U.Table.create ();
      chan_of_id = Chan_id.Table.create ();
    }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t chan =
    Hashtbl.find_or_add t.id_of_chan chan ~default:(fun () ->
        let id = new_id t in
        Hashtbl.set t.chan_of_id ~key:id ~data:chan;
        id)

  let find_exn t chan = Hashtbl.find_exn t.id_of_chan chan
  let ct t = t.next_id
  let chan_of_id t id = Hashtbl.find_exn t.chan_of_id id
end

module Mem_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_mem : (Var_id.t * Mem_id.t) Ir.Mem.Table.t;
  }

  let create () = { next_id = 0; id_of_mem = Ir.Mem.Table.create () }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t var_id_pool mem =
    Hashtbl.find_or_add t.id_of_mem mem ~default:(fun () ->
        let mem_idx = new_id t in
        let helper_reg_var_idx =
          Var_id_pool.new_id var_id_pool Mem_idx_reg Ir.DType.dummy_val
        in
        (helper_reg_var_idx, mem_idx))

  let id_of_mem t = t.id_of_mem
end

let create ?(seed = 0) ir ~user_sendable_ports ~user_readable_ports =
  let ir = Ir.N.unwrap ir in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);

  let ab = Assem_builder.create () in
  let push_instr loc instr = Assem_builder.push ab loc instr in
  let edit_instr loc idx instr = Assem_builder.edit ab loc idx instr in

  let var_id_pool = Var_id_pool.create () in
  let convert_id id = Var_id_pool.to_assem_id var_id_pool id in
  let convert_expr expr =
    Var_id_pool.to_assem_expr var_id_pool (Ir.Expr.untype expr)
  in

  let chan_id_pool = Chan_id_pool.create () in
  let get_chan chan_id = Chan_id_pool.get_id chan_id_pool chan_id in

  let mem_id_pool = Mem_id_pool.create () in
  let get_mem mem =
    Mem_id_pool.get_id mem_id_pool var_id_pool mem
    (* Hashtbl.find_or_add mem_tbl mem ~default:(fun () ->
        Mem_buff.create ~init:mem.d.init
          ~idx_helper_reg:(Var_id_pool.new_id var_id_pool Mem_idx_reg)) *)
  in

  let rec convert_stmt stmt =
    let convert' stmt = ignore (convert_stmt stmt : Instr_idx.t) in
    let push_branches loc stmts =
      let split = push_instr loc Nop in
      let ends =
        List.map stmts ~f:(fun stmt ->
            convert' stmt;
            push_instr loc (Jump Instr_idx.dummy_val))
      in
      let starts =
        List.take (split :: ends) (List.length stmts)
        |> List.map ~f:Instr_idx.next
      in
      let merge = push_instr loc Nop in
      List.iter ends ~f:(fun end_ -> edit_instr loc end_ (Jump merge));
      (split, starts, merge)
    in
    match stmt with
    | Ir.N.Assign (loc, id, expr) ->
        push_instr loc (Assign (convert_id id, convert_expr expr))
    | Log (loc, str) -> push_instr loc (Log0 str)
    | Log1 (loc, expr, f) -> push_instr loc (Log1 (convert_expr expr, f))
    | Assert (loc, expr) -> push_instr loc (Assert (convert_expr expr))
    | Seq (loc, stmts) -> (
        match stmts with
        | [] -> push_instr loc Nop
        | stmts -> List.map stmts ~f:convert_stmt |> List.last_exn)
    | Par (loc, stmts) ->
        let split, starts, merge = push_branches loc stmts in
        edit_instr loc split (Par starts);
        edit_instr loc merge
          (ParJoin (Par_join.create ~max_ct:(List.length stmts)));
        merge
    | SelectImm (loc, branches, else_) -> (
        match else_ with
        | Some else_ ->
            let guards, stmts = List.unzip branches in
            let split, starts, merge = push_branches loc (else_ :: stmts) in
            let guards = List.map guards ~f:convert_expr in
            let guards = List.zip_exn guards (List.tl_exn starts) in
            edit_instr loc split (SelectImmElse (guards, List.hd_exn starts));
            merge
        | None ->
            let guards, stmts = List.unzip branches in
            let split, starts, merge = push_branches loc stmts in
            let guards = List.map guards ~f:convert_expr in
            let guards = List.zip_exn guards starts in
            edit_instr loc split (SelectImm guards);
            merge)
    | Read (loc, chan, var) ->
        let chan_idx = get_chan chan in
        push_instr loc (Read (convert_id var, chan_idx))
    | Send (loc, chan, expr) ->
        let chan_idx = get_chan chan in
        push_instr loc (Send (convert_expr expr, chan_idx))
    | Loop (loc, seq) ->
        let fst = Assem_builder.next_idx ab in
        convert' seq;
        push_instr loc (Jump fst)
    | WhileLoop (loc, expr, seq) ->
        let split =
          push_instr loc (JumpIfFalse (convert_expr expr, Instr_idx.dummy_val))
        in
        convert' seq;
        let jmp = push_instr loc (Jump split) in
        edit_instr loc split
          (JumpIfFalse (convert_expr expr, Instr_idx.next jmp));
        jmp
    | ReadUGMem (loc, mem, idx, dst) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr loc
          (ReadMem (convert_expr idx, convert_id dst, mem_idx_reg, mem_id))
    | WriteUGMem (loc, mem, idx, value) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr loc
          (WriteMem (convert_expr idx, convert_expr value, mem_idx_reg, mem_id))
    | WaitUntilReadReady (loc, chan) ->
        let chan_idx = get_chan chan in
        let (_ : Instr_idx.t) = push_instr loc (WaitUntilReadReady chan_idx) in
        push_instr loc (AssertStillReadReady chan_idx)
    | WaitUntilSendReady (loc, chan) ->
        let chan_idx = get_chan chan in
        let (_ : Instr_idx.t) = push_instr loc (WaitUntilSendReady chan_idx) in
        push_instr loc (AssertStillSendReady chan_idx)
  in

  (* Build the main program. An initial jump is required. *)
  let () =
    let init_jump = push_instr Code_pos.dummy_loc (Jump Instr_idx.dummy_val) in
    let start = Assem_builder.next_idx ab in
    edit_instr Code_pos.dummy_loc init_jump (Jump start)
  in
  let (_ : Instr_idx.t) = convert_stmt ir in
  let (_ : Instr_idx.t) = push_instr Code_pos.dummy_loc End in

  (* set up user enqueuers *)
  let push_dequeuer chan =
    let chan_idx = Chan_id_pool.find_exn chan_id_pool chan in
    let var_id = Var_id_pool.new_id var_id_pool Read_deq_reg chan.d.dtype in
    let read_instr = push_instr Code_pos.dummy_loc (Read (var_id, chan_idx)) in
    let dequeuer =
      Read_dequeuer.create ~var_id
        ~equals:(Ir.DType.equal_fn chan.d.dtype |> Staged.unstage)
        chan_idx
    in
    let _ = push_instr Code_pos.dummy_loc (Read_dequeuer dequeuer) in
    (chan, (read_instr, dequeuer))
  in
  let all_dequeuers =
    Set.to_list user_readable_ports |> List.map ~f:push_dequeuer
  in
  let all_dequeuers = Ir.Chan.U.Map.of_alist_exn all_dequeuers in

  (* set up user dequeuers *)
  let push_enqueuer chan =
    let chan_idx = Chan_id_pool.find_exn chan_id_pool chan in
    let var_id = Var_id_pool.new_id var_id_pool Send_enq_reg chan.d.dtype in
    let send_instr =
      push_instr Code_pos.dummy_loc (Send (Var var_id, chan_idx))
    in
    let enqueuer = Send_enqueuer.create ~var_id chan_idx in
    let _ = push_instr Code_pos.dummy_loc (Send_enqueuer enqueuer) in
    (chan, (send_instr, enqueuer))
  in
  let all_enqueuers =
    Set.to_list user_sendable_ports |> List.map ~f:push_enqueuer
  in
  let all_enqueuers = Ir.Chan.U.Map.of_alist_exn all_enqueuers in

  let assem = Assem_builder.assem_array ab in
  let assem, loc_of_assem_idx = Array.unzip assem in
  let var_table =
    let var_inits =
      Var_id_pool.var_inits var_id_pool
      |> Hashtbl.to_alist
      |> List.sort ~compare:(fun (id1, _) (id2, _) -> Int.compare id1 id2)
    in
    List.iteri var_inits ~f:(fun i (id, _) -> assert (Int.equal i id));
    List.map var_inits ~f:(fun (_, (dtype, src)) ->
        let value, is_inited =
          match src with
          | Var_id_src.Mem_idx_reg | Read_deq_reg | Send_enq_reg ->
              (Obj.magic 0, false)
          | Var var -> (
              match var.Ir.Var.U.d.init with
              | Some value -> (value, true)
              | None -> (Obj.magic 0, false))
        in
        { Var_buff.src; dtype; value; is_inited; read_ct = 0; write_ct = 0 })
    |> Array.of_list
  in
  let pcs = Vec.of_array [| 0 |] ~default:(-1) in
  let rng = Random.State.make [| seed |] in

  let chan_table =
    Array.init (Chan_id_pool.ct chan_id_pool) ~f:(fun id ->
        let chan = Chan_id_pool.chan_of_id chan_id_pool id in
        Chan_buff.create chan)
  in
  let mem_table =
    let tbl =
      Mem_id_pool.id_of_mem mem_id_pool
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (mem, (idx_reg, mem_id)) -> (mem_id, (idx_reg, mem)))
      |> List.sort ~compare:(fun (id0, _) (id1, _) -> Int.compare id0 id1)
    in
    List.iteri tbl ~f:(fun i (mem_id, _) -> assert (Int.equal i mem_id));
    List.map tbl ~f:snd
    |> List.map ~f:(fun (idx_helper_reg, mem) ->
           Mem_buff.create ~idx_helper_reg mem)
    |> Array.of_list
  in
  {
    assem;
    loc_of_assem_idx;
    all_enqueuers;
    all_dequeuers;
    queued_user_ops = Queue.create ();
    pcs;
    var_table;
    chan_table;
    mem_table;
    rng;
    is_done = false;
  }

let send t ?loc chan value =
  let chan = Ir.Chan.unwrap_w chan in
  let value = Any.of_magic value in
  (match check_value_fits_in_dtype chan.d.dtype ~value ~error:() with
  | Ok () -> ()
  | Error () ->
      failwith
        [%string
          "Sent value doesnt fit in chan: got value %{Ir.DType.sexp_of_t_ \
           chan.d.dtype value#Sexp} but channel has layout %{Ir.DType.layout \
           chan.d.dtype |> Ir.Layout.sexp_of_t#Sexp}."]);
  let call_site = Code_pos.value_or_psite loc in
  match Map.find t.all_enqueuers chan with
  | Some (send_instr, enqueuer) ->
      Queue.enqueue t.queued_user_ops
        {
          Queued_user_op.queuer = `Send enqueuer;
          chan_instr = send_instr;
          value;
          call_site;
        }
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-sendable chan in \
         Sim.create"

let read t ?loc chan value =
  let chan = Ir.Chan.unwrap_r chan in
  let value = Any.of_magic value in
  (match check_value_fits_in_dtype chan.d.dtype ~value ~error:() with
  | Ok () -> ()
  | Error () ->
      failwith
        [%string
          "Read value doesnt fit in chan: got value %{Ir.DType.sexp_of_t_ \
           chan.d.dtype value#Sexp} but channel has layout %{Ir.DType.layout \
           chan.d.dtype |> Ir.Layout.sexp_of_t#Sexp}."]);
  let call_site = Code_pos.value_or_psite loc in
  match Map.find t.all_dequeuers chan with
  | Some (read_instr, dequeuer) ->
      Queue.enqueue t.queued_user_ops
        {
          Queued_user_op.queuer = `Read dequeuer;
          chan_instr = read_instr;
          value;
          call_site;
        }
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-readable chan in \
         Sim.create"
