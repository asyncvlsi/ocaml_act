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
module Enqueuer_idx = Int
module Dequeuer_idx = Int

module Probe = struct
  type t = Read_ready of Chan_id.t | Send_ready of Chan_id.t
  [@@deriving sexp_of, equal]
end

module Expr = struct
  type t =
    | Var of Var_id.t
    | Const of CInt.t
    | Map of t * (CInt.t -> CInt.t)
    | Map2 of t * t * (CInt.t -> CInt.t -> CInt.t)
    | AssertThenOrLog0 of t * t * (unit -> string)
    | AssertThenOrLog1 of t * t * t * (CInt.t -> string)
    | AssertThenOrLog2 of t * t * t * t * (CInt.t -> CInt.t -> string)
  [@@deriving sexp_of]

  let map e ~f = Map (e, f)
  let map2 e1 e2 ~f = Map2 (e1, e2, f)

  let _assert_then_or_log0 assert_e val_e log_fn =
    AssertThenOrLog0 (assert_e, val_e, log_fn)

  let assert_then_or_log1 assert_e val_e log_e1 log_fn =
    AssertThenOrLog1 (assert_e, val_e, log_e1, log_fn)

  let assert_then_or_log2 assert_e val_e log_e1 log_e2 log_fn =
    AssertThenOrLog2 (assert_e, val_e, log_e1, log_e2, log_fn)

  let rec var_ids t =
    match t with
    | Var id -> [ id ]
    | Const _ -> []
    | Map (e, _) -> var_ids e
    | Map2 (e1, e2, _) -> var_ids e1 @ var_ids e2
    | AssertThenOrLog0 (ae, ve, _) -> var_ids ae @ var_ids ve
    | AssertThenOrLog1 (ae, ve, le1, _) -> var_ids ae @ var_ids ve @ var_ids le1
    | AssertThenOrLog2 (ae, ve, le1, le2, _) ->
        var_ids ae @ var_ids ve @ var_ids le1 @ var_ids le2
end

module Var_id_src = struct
  type t = Var of Ir.Var.U.t | Mem_idx_reg | Read_deq_reg | Send_enq_reg
  [@@deriving sexp_of]
end

module Var_buff = struct
  type t = {
    (* immutable data *)
    bitwidth : int;
    (* mutable *)
    mutable value : CInt.t;
    mutable is_inited : bool;
    mutable read_ct : int;
    mutable write_ct : int;
  }
  [@@deriving sexp_of]
end

module Var_buff_info = struct
  type t = { src : Var_id_src.t; dtype : Any.t Ir.DType.t }
end

module Chan_buff = struct
  type t = {
    (* immutable data *)
    bitwidth : int;
    (* mutable *)
    mutable read_ready : bool;
    mutable read_instr : Instr_idx.t;
    mutable read_dst_var_id : Var_id.t;
    mutable send_ready : bool;
    mutable send_instr : Instr_idx.t;
    mutable send_expr : Expr.t;
    select_probe_read_ready :
      (Instr_idx.t * (Probe.t * Instr_idx.t) list) Vec.t;
    select_probe_send_ready : (Instr_idx.t * (Probe.t * Instr_idx.t) list) Vec.t;
  }
  [@@deriving sexp_of]

  let create bitwidth =
    {
      bitwidth;
      read_ready = false;
      send_ready = false;
      read_instr = Instr_idx.dummy_val;
      send_instr = Instr_idx.dummy_val;
      read_dst_var_id = 0;
      send_expr = Const (Obj.magic 0);
      select_probe_read_ready = Vec.create ~cap:10 ~default:(0, []);
      select_probe_send_ready = Vec.create ~cap:10 ~default:(0, []);
    }
end

module Chan_buff_info = struct
  type t = { src : (Ir.Chan.U.t[@sexp.opaque]) } [@@deriving sexp_of]
end

module Mem_buff = struct
  type t = {
    (* immutable data *)
    cell_bitwidth : int;
    (* mutable *)
    arr : CInt.t array;
    idx_helper_reg : Var_id.t;
  }
  [@@deriving sexp_of]

  let create ~idx_helper_reg src =
    let arr =
      Array.map src.Ir.Mem.d.init ~f:(fun v ->
          Ir.DType.cint_of_value src.d.dtype v)
    in
    let cell_bitwidth =
      match Ir.DType.layout src.d.dtype with Bits_fixed bitwidth -> bitwidth
    in
    { cell_bitwidth; arr; idx_helper_reg }
end

module Mem_buff_info = struct
  type t = { src : (Ir.Mem.t[@sexp.opaque]) } [@@deriving sexp_of]
end

module Enqueuer_buff = struct
  type t = {
    mutable to_send : CInt.t With_origin.t array;
    var_id : Var_id.t;
    mutable is_done : bool;
    mutable idx : int;
  }
  [@@deriving sexp_of]

  let create ~var_id = { to_send = [||]; var_id; is_done = true; idx = 0 }
end

module Enqueuer_info = struct
  type t = { chan : Chan_id.t } [@@deriving sexp_of]
end

module Dequeuer_buff = struct
  type t = {
    mutable expected_reads : CInt.t With_origin.t array;
    var_id : Var_id.t;
    mutable idx : int;
  }
  [@@deriving sexp_of]

  let create ~var_id = { expected_reads = [||]; var_id; idx = 0 }
end

module Dequeuer_info = struct
  type t = { chan : Chan_id.t } [@@deriving sexp_of]
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
    | Log1 of Expr.t * (CInt.t -> string)
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
    (* handle nondeterministic select *)
    | SelectProbes of (Probe.t * Instr_idx.t) list
    (* Should include all but the branch just taken *)
    | SelectProbes_AssertStable of
        (* should be true *) Probe.t * (* should be false *) Probe.t list
    (* These are ``magic'' instructions that allow user io operations. These instruction
       should be placed immediatly after the assoiated send/read instruction *)
    | Send_enqueuer of Enqueuer_idx.t
    | Read_dequeuer of Dequeuer_idx.t
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
    | SelectProbes _ | SelectProbes_AssertStable _ -> [])
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
    | SelectProbes _ | SelectProbes_AssertStable _ -> [])
    |> Var_id.Set.of_list
end

(* This module doesnt know about dtypes. Everything is just a CInt. This should be fairly easy to port
   into c/c++/rust if we need the extra performance *)
module Inner_data = struct
  type t = {
    assem : N.t array;
    (* simulation state *)
    pcs : Instr_idx.t Vec.t;
    var_table : Var_buff.t array;
    chan_table : Chan_buff.t array;
    mem_table : Mem_buff.t array;
    enqueuer_table : Enqueuer_buff.t array;
    dequeuer_table : Dequeuer_buff.t array;
    rng : (Random.State.t[@sexp.opaque]);
  }

  let set_enqueuer t ~enqueuer_idx ~is_done ~idx ~to_send ~push_pc =
    let enqueuer = t.enqueuer_table.(enqueuer_idx) in
    enqueuer.is_done <- is_done;
    enqueuer.idx <- idx;
    enqueuer.to_send <- to_send;
    Vec.push t.pcs push_pc

  let set_dequeuer t ~dequeuer_idx ~idx ~expected_reads ~push_pc =
    let dequeuer = t.dequeuer_table.(dequeuer_idx) in
    dequeuer.idx <- idx;
    dequeuer.expected_reads <- expected_reads;
    Vec.push t.pcs push_pc

  let check_value_fits_width width ~value ~error =
    if width >= CInt.bitwidth value then Ok () else Error error

  let step' t ~pc_idx =
    let bool_of_cint i =
      match CInt.to_int_exn i with
      | 0 -> false
      | 1 -> true
      | c -> failwith [%string "Simulator bug: unexpected bool value %{c#Int}"]
    in
    let eval_var_table expr ~(on_error : string -> 'b) : (CInt.t, 'b) result =
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
        | AssertThenOrLog0 (assert_e, val_e, log_fn) -> (
            let%bind.Result assert_e =
              eval assert_e |> Result.map ~f:bool_of_cint
            in
            match assert_e with
            | true -> eval val_e
            | false ->
                let raw_error = log_fn () in
                Error (on_error raw_error))
        | AssertThenOrLog1 (assert_e, val_e, log_e1, log_fn) -> (
            let%bind.Result assert_e =
              eval assert_e |> Result.map ~f:bool_of_cint
            in
            match assert_e with
            | true -> eval val_e
            | false ->
                let%bind.Result log_e1 = eval log_e1 in
                let raw_error = log_fn log_e1 in
                Error (on_error raw_error))
        | AssertThenOrLog2 (assert_e, val_e, log_e1, log_e2, log_fn) -> (
            let%bind.Result assert_e =
              eval assert_e |> Result.map ~f:bool_of_cint
            in
            match assert_e with
            | true -> eval val_e
            | false ->
                let%bind.Result log_e1 = eval log_e1 in
                let%bind.Result log_e2 = eval log_e2 in
                let raw_error = log_fn log_e1 log_e2 in
                Error (on_error raw_error))
      in
      eval expr
    in
    let eval_bool expr ~on_error =
      eval_var_table expr ~on_error |> Result.map ~f:bool_of_cint
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
    in
    let find_reader var_id ~ignore =
      find_rw t var_id ~ignore ~get_ids:N.read_ids
    in
    let find_writer var_id ~ignore =
      find_rw t var_id ~ignore ~get_ids:N.write_ids
    in
    let guard pc =
      let read_ids = N.read_ids t.assem.(pc) in
      let write_ids = N.write_ids t.assem.(pc) in
      let%bind.Result () =
        Set.find read_ids ~f:(fun read_id -> t.var_table.(read_id).write_ct > 0)
        |> to_unit_result ~f:(fun var_id ->
               let write_pc = find_writer var_id ~ignore:pc in
               `Simul_read_write_var (pc, write_pc, var_id))
      in
      let%bind.Result () =
        Set.find read_ids ~f:(fun read_id ->
            not t.var_table.(read_id).is_inited)
        |> to_unit_result ~f:(fun read_id -> `Uninit_id (read_id, pc))
      in
      let%bind.Result () =
        Set.find write_ids ~f:(fun write_id ->
            t.var_table.(write_id).read_ct > 0)
        |> to_unit_result ~f:(fun var_id ->
               let read_pc = find_reader var_id ~ignore:pc in
               `Simul_read_write_var (read_pc, pc, var_id))
      in
      let%map.Result () =
        Set.find write_ids ~f:(fun write_id ->
            t.var_table.(write_id).write_ct > 0)
        |> to_unit_result ~f:(fun var_id ->
               let write_pc_2 = find_writer var_id ~ignore:pc in
               `Simul_write_write_var (pc, write_pc_2, var_id))
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

    let step_chan (chan : Chan_buff.t) chan_idx =
      if chan.read_ready && chan.send_ready then (
        chan.read_ready <- false;
        chan.send_ready <- false;
        unguard chan.read_instr;
        unguard chan.send_instr;
        let%bind.Result value =
          eval_var_table chan.send_expr ~on_error:(fun error_string ->
              `Eval_expr_failed (`Send, chan.send_instr, error_string))
        in
        let%bind.Result () =
          check_value_fits_width chan.bitwidth ~value
            ~error:
              (`Sent_value_doesnt_fit_in_chan
                (chan.send_instr, chan_idx, value))
        in
        let%bind.Result () =
          let var_width = t.var_table.(chan.read_dst_var_id).bitwidth in
          check_value_fits_width var_width ~value
            ~error:
              (`Read_chan_value_doesnt_fit_in_var
                (chan.read_instr, chan.read_dst_var_id, value))
        in
        let () = set_var_table ~var_id:chan.read_dst_var_id ~value in
        let () =
          Vec.extend t.pcs [ chan.read_instr + 1; chan.send_instr + 1 ]
        in
        let%bind.Result () = guard (chan.read_instr + 1) in
        guard (chan.send_instr + 1))
      else Ok ()
    in

    let step_select l ~else_ ~pc ~pc_idx =
      let%bind.Result branches =
        List.mapi l ~f:(fun idx (expr, instr) ->
            let%map.Result guard =
              eval_bool expr ~on_error:(fun error_string ->
                  `Eval_expr_failed (`Guard idx, pc, error_string))
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
              `Eval_expr_failed (`Assign, pc, error_string))
        in
        let%bind.Result () =
          let var_width = t.var_table.(var_id).bitwidth in
          check_value_fits_width var_width ~value
            ~error:(`Assigned_value_doesnt_fit_in_var (pc, var_id, value))
        in
        let () = set_var_table ~var_id ~value in
        set_pc_and_guard ~pc_idx (pc + 1)
    | Assert expr -> (
        unguard pc;
        let%bind.Result expr =
          eval_bool expr ~on_error:(fun error_string ->
              `Eval_expr_failed (`Assert, pc, error_string))
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
              `Eval_expr_failed (`Log1, pc, error_string))
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
              `Eval_expr_failed (`Jump_if_false, pc, error_string))
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
        if chan.read_ready then
          Error (`Simul_chan_readers (chan.read_instr, pc))
        else (
          chan.read_ready <- true;
          chan.read_instr <- pc;
          chan.read_dst_var_id <- dst_id;
          Vec.remove t.pcs pc_idx;
          Vec.iter chan.select_probe_read_ready
            ~f:(fun (waiting_pc, other_probes) ->
              (* First go and turn off all the other probes! *)
              List.iter other_probes ~f:(fun (oprobe, oinstr) ->
                  match oprobe with
                  | Read_ready ochan_idx ->
                      assert (not (Int.equal ochan_idx chan_idx));
                      Vec.filter
                        t.chan_table.(ochan_idx).select_probe_read_ready
                        ~f:(fun (pc, _) -> not (Int.equal pc oinstr))
                  | Send_ready ochan_idx ->
                      assert (not (Int.equal ochan_idx chan_idx));
                      Vec.filter
                        t.chan_table.(ochan_idx).select_probe_send_ready
                        ~f:(fun (pc, _) -> not (Int.equal pc oinstr)));
              (* It doesnt need gaurding becaus the waiting_pc is required to be a WaitUntilReadable
                 or WaitUntilSendable node, which has no read/written variables *)
              Vec.push t.pcs waiting_pc);
          step_chan chan chan_idx)
    | Send (expr, chan_idx) ->
        (* unguard pc; *)
        let chan = t.chan_table.(chan_idx) in
        if chan.send_ready then
          Error (`Simul_chan_senders (chan.send_instr, pc))
        else (
          chan.send_ready <- true;
          chan.send_instr <- pc;
          chan.send_expr <- expr;
          Vec.remove t.pcs pc_idx;
          Vec.iter chan.select_probe_send_ready
            ~f:(fun (waiting_pc, other_probes) ->
              (* First go and turn off all the other probes! *)
              List.iter other_probes ~f:(fun (oprobe, oinstr) ->
                  match oprobe with
                  | Read_ready ochan_idx ->
                      assert (not (Int.equal ochan_idx chan_idx));
                      Vec.filter
                        t.chan_table.(ochan_idx).select_probe_read_ready
                        ~f:(fun (pc, _) -> not (Int.equal pc oinstr))
                  | Send_ready ochan_idx ->
                      assert (not (Int.equal ochan_idx chan_idx));
                      Vec.filter
                        t.chan_table.(ochan_idx).select_probe_send_ready
                        ~f:(fun (pc, _) -> not (Int.equal pc oinstr)));
              (* It doesnt need gaurding becaus the waiting_pc is required to be a WaitUntilReadable
                 or WaitUntilSendable node, which has no read/written variables *)
              Vec.push t.pcs waiting_pc);
          step_chan chan chan_idx)
    | SelectProbes probe_select -> (
        (* TODO *)
        (* unguard pc; *)
        (* first check that how many probes are already true. If it is more than one, this is an error *)
        match
          List.mapi probe_select ~f:(fun i probe -> (i, probe))
          |> List.filter ~f:(fun (_, (probe, _)) ->
                 match probe with
                 | Read_ready chan_idx -> t.chan_table.(chan_idx).read_ready
                 | Send_ready chan_idx -> t.chan_table.(chan_idx).send_ready)
        with
        | [] ->
            Vec.remove t.pcs pc_idx;
            List.iter probe_select ~f:(fun (probe, instr) ->
                let other_instrs =
                  List.filter probe_select ~f:(fun (_, i) ->
                      not (Int.equal i instr))
                in
                match probe with
                | Read_ready chan_idx ->
                    Vec.push t.chan_table.(chan_idx).select_probe_read_ready
                      (instr, other_instrs)
                | Send_ready chan_idx ->
                    Vec.push t.chan_table.(chan_idx).select_probe_send_ready
                      (instr, other_instrs));
            Ok ()
        | [ (_, (_, instr)) ] -> set_pc_and_guard ~pc_idx instr
        | multiple_probes ->
            Error (`Select_multiple_true_probes (pc, multiple_probes)))
    | SelectProbes_AssertStable (tprobe, fprobes) ->
        (* unguard pc; *)
        let errors =
          (let is_ready =
             match tprobe with
             | Read_ready chan_idx -> t.chan_table.(chan_idx).read_ready
             | Send_ready chan_idx -> t.chan_table.(chan_idx).send_ready
           in
           if is_ready then Ok () else Error (`Unstable_probe (pc, tprobe)))
          :: List.map fprobes ~f:(fun probe ->
                 let is_ready =
                   match probe with
                   | Read_ready chan_idx -> t.chan_table.(chan_idx).read_ready
                   | Send_ready chan_idx -> t.chan_table.(chan_idx).send_ready
                 in
                 if is_ready then Error (`Unstable_probe (pc, probe)) else Ok ())
        in
        let%bind.Result () = Result.all_unit errors in
        set_pc_and_guard ~pc_idx (pc + 1)
    | Send_enqueuer enq_idx ->
        (* unguard pc; *)
        let enqueuer = t.enqueuer_table.(enq_idx) in
        assert (not enqueuer.is_done);
        if enqueuer.idx >= Array.length enqueuer.to_send then (
          Vec.remove t.pcs pc_idx;
          enqueuer.is_done <- true;
          Ok ())
        else
          let { With_origin.value; origin = _ } =
            enqueuer.to_send.(enqueuer.idx)
          in
          enqueuer.idx <- enqueuer.idx + 1;
          set_var_table ~var_id:enqueuer.var_id ~value;
          set_pc_and_guard ~pc_idx (pc - 1)
    | Read_dequeuer deq_idx ->
        let dequeuer = t.dequeuer_table.(deq_idx) in
        unguard pc;
        let value = at_var_table ~var_id:dequeuer.var_id in
        let expected = dequeuer.expected_reads.(dequeuer.idx) in
        if not (CInt.equal value expected.value) then
          Error
            (`Read_dequeuer_wrong_value
              (deq_idx, value, expected, dequeuer.idx))
        else (
          dequeuer.idx <- dequeuer.idx + 1;
          if dequeuer.idx >= Array.length dequeuer.expected_reads then (
            Vec.remove t.pcs pc_idx;
            Ok ())
          else set_pc_and_guard ~pc_idx (pc - 1))
    | ReadMem (idx_expr, dst_id, _, mem_idx) ->
        unguard pc;
        let mem = t.mem_table.(mem_idx) in
        let%bind.Result idx =
          eval_var_table idx_expr ~on_error:(fun error_string ->
              `Eval_expr_failed (`Mem_idx, pc, error_string))
        in
        if CInt.(idx < zero) || CInt.(idx >= (Array.length mem.arr |> of_int))
        then Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
        else
          let value = mem.arr.(CInt.to_int_exn idx) in
          let%bind.Result () =
            let var_width = t.var_table.(dst_id).bitwidth in
            check_value_fits_width var_width ~value
              ~error:(`Read_mem_value_doesnt_fit_in_var (pc, dst_id, value))
          in
          let () =
            set_var_table ~var_id:dst_id ~value:mem.arr.(CInt.to_int_exn idx)
          in
          set_pc_and_guard ~pc_idx (pc + 1)
    | WriteMem (idx_expr, src_expr, _, mem_idx) ->
        unguard pc;
        let mem = t.mem_table.(mem_idx) in
        let%bind.Result idx =
          eval_var_table idx_expr ~on_error:(fun error_string ->
              `Eval_expr_failed (`Mem_idx, pc, error_string))
        in
        if CInt.(idx < zero) || CInt.(idx >= (Array.length mem.arr |> of_int))
        then Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
        else
          let%bind.Result value =
            eval_var_table src_expr ~on_error:(fun error_string ->
                `Eval_expr_failed (`Write_mem_value, pc, error_string))
          in
          let%bind.Result () =
            check_value_fits_width mem.cell_bitwidth ~value
              ~error:
                (`Written_mem_value_doesnt_fit_in_cell (pc, mem_idx, value))
          in
          let () = mem.arr.(CInt.to_int_exn idx) <- value in
          set_pc_and_guard ~pc_idx (pc + 1)

  let step t =
    if Vec.is_empty t.pcs then Error `Stuck
    else step' t ~pc_idx:(Random.State.int_incl t.rng 0 (Vec.length t.pcs - 1))

  let wait t ~max_steps () =
    for_loop_else max_steps
      ~f:(fun _ ->
        let status = step t in
        match status with
        | Ok () -> `Continue
        | Error `Stuck -> (
            (* check whether we need error because the queuers are unfinished *)
            match
              Array.findi t.dequeuer_table ~f:(fun _ deq ->
                  deq.idx < Array.length deq.expected_reads)
            with
            | Some (deq_idx, deq) ->
                let read_idx = deq.idx in
                let read = deq.expected_reads.(read_idx) in
                `Return (`User_read_did_not_complete (deq_idx, read))
            | None -> (
                match
                  Array.findi t.enqueuer_table ~f:(fun _ enq -> not enq.is_done)
                with
                | Some (enq_idx, enq) ->
                    let send_idx = enq.idx in
                    let send = enq.to_send.(send_idx - 1) in
                    `Return (`User_send_did_not_complete (enq_idx, send))
                | None -> `Return `Stuck))
        | Error e -> `Return e)
      ~else_:`Time_out
end

module Queued_user_op = struct
  type t = {
    queuer : [ `Send of Enqueuer_idx.t | `Read of Dequeuer_idx.t ];
    chan_instr : Instr_idx.t;
    value : CInt.t;
    call_site : Code_pos.t;
  }
end

type t = {
  i : Inner_data.t;
  mutable is_done : bool;
  (* error message helpers *)
  loc_of_assem_idx : Code_pos.t array;
  var_table_info : Var_buff_info.t array;
  chan_table_info : Chan_buff_info.t array;
  mem_table_info : Mem_buff_info.t array;
  enqueuer_table_info : Enqueuer_info.t array;
  dequeuer_table_info : Dequeuer_info.t array;
  (* io helpers *)
  all_enqueuers : (Instr_idx.t * Enqueuer_idx.t) Ir.Chan.U.Map.t;
  all_dequeuers : (Instr_idx.t * Dequeuer_idx.t) Ir.Chan.U.Map.t;
  (* per-wait state *)
  queued_user_ops : Queued_user_op.t Queue.t;
}

let resolve_step_err t e ~line_numbers =
  let loc_of_instr var_id = t.loc_of_assem_idx.(var_id) in
  let str_l (cp : Code_pos.t) =
    if line_numbers then
      [%string "in %{cp.filename} on line %{cp.line_number#Int}"]
    else "<loc>"
  in
  let str_i pc = loc_of_instr pc |> str_l in
  match e with
  | `Stuck -> Ok `Stuck
  | `Time_out -> Error "Simulation timed out. Maybe increase max_steps?"
  | `Already_done -> Error "Already done."
  | `User_read_did_not_complete (deq_idx, read) ->
      let chan_idx = t.dequeuer_table_info.(deq_idx).chan in
      let chan_creation_pos =
        t.chan_table_info.(chan_idx).src.d.creation_code_pos
      in
      Error
        [%string
          "User read did not complete:  called %{str_l \
           read.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | `User_send_did_not_complete (enq_idx, send) ->
      let chan_idx = t.enqueuer_table_info.(enq_idx).chan in
      let chan_creation_pos =
        t.chan_table_info.(chan_idx).src.d.creation_code_pos
      in
      Error
        [%string
          "User send did not complete:  called %{str_l \
           send.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | `Uninit_id (var_id, pc) ->
      let var_code_pos =
        match t.var_table_info.(var_id).src with
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
  | `Simul_read_write_var (read_pc, write_pc, var_id) ->
      let var =
        match t.var_table_info.(var_id).src with
        | Var var -> var
        | Mem_idx_reg | Read_deq_reg | Send_enq_reg ->
            failwith "should be unreachable"
      in
      let var_decl = var.d.creation_code_pos in
      Error
        [%string
          "Simultanious read and write of variable: read %{str_i read_pc}, \
           write %{str_i write_pc}, create %{str_l var_decl}."]
  | `Simul_write_write_var (pc1, pc2, var_id) -> (
      (* This could either be because an actual variable is written in two locations,
         or could be the fake variable used by a memory to mark that two accesses are
         happening at the same time *)
      match t.var_table_info.(var_id).src with
      | Var var ->
          Error
            [%string
              "Simulatnious writes of variable: statement 1 %{str_i pc1}, \
               statement 2 %{str_i pc2}, create %{str_l \
               var.d.creation_code_pos}."]
      | Mem_idx_reg ->
          Error
            [%string
              "Simulatnious accesses of a memory/rom: statement 1 %{str_i \
               pc1}, statement 2 %{str_i pc2}."]
      | Read_deq_reg | Send_enq_reg -> failwith "unreachable")
  | `Select_no_guards_true pc ->
      Error [%string "Select statement has no true guards: %{str_i pc}."]
  | `Select_multiple_guards_true (pc, branch_idxs) ->
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true guards: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]
  | `Read_dequeuer_wrong_value (deq_idx, actual, expected, _) ->
      let chan_idx = t.dequeuer_table_info.(deq_idx).chan in
      let chan_dtype = t.chan_table_info.(chan_idx).src.d.dtype in
      let actual =
        Ir.DType.value_of_cint_exn chan_dtype actual
        |> Ir.DType.sexp_of_t_ chan_dtype
      in
      let expected =
        With_origin.map expected ~f:(fun expected ->
            Ir.DType.value_of_cint_exn chan_dtype expected
            |> Ir.DType.sexp_of_t_ chan_dtype)
      in
      let chan_decl = t.chan_table_info.(chan_idx).src.d.creation_code_pos in
      Error
        [%string
          "User read has wrong value: got %{actual#Sexp}, but expected \
           %{expected.With_origin.value#Sexp} based on `send' function call \
           %{str_l expected.origin}, on chan created %{str_l chan_decl}."]
  | `Mem_out_of_bounds (pc, idx, len) ->
      Error
        [%string
          "Mem access out of bounds: %{str_i pc}, idx is %{idx#CInt}, size of \
           mem is %{len#Int}."]
  | `Assigned_value_doesnt_fit_in_var (assign_instr, var_id, value) ->
      let var_dtype = t.var_table_info.(var_id).dtype in
      let var_layout = Ir.DType.layout var_dtype in
      let value =
        Ir.DType.value_of_cint_exn var_dtype value
        |> Ir.DType.sexp_of_t_ var_dtype
      in
      Error
        [%string
          "Assigned value doesnt fit in var: got %{value#Sexp} but variable \
           has layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           assign_instr}."]
  | `Read_chan_value_doesnt_fit_in_var (read_instr, read_dst_var_id, value) ->
      let var_dtype = t.var_table_info.(read_dst_var_id).dtype in
      let var_layout = Ir.DType.layout var_dtype in
      let value =
        Ir.DType.value_of_cint_exn var_dtype value
        |> Ir.DType.sexp_of_t_ var_dtype
      in
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           read_instr}."]
  | `Read_mem_value_doesnt_fit_in_var (read_instr, dst_id, value) ->
      let var_dtype = t.var_table_info.(dst_id).dtype in
      let var_layout = Ir.DType.layout var_dtype in
      let value =
        Ir.DType.value_of_cint_exn var_dtype value
        |> Ir.DType.sexp_of_t_ var_dtype
      in
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout %{Ir.Layout.sexp_of_t var_layout#Sexp} at %{str_i \
           read_instr}."]
  | `Sent_value_doesnt_fit_in_chan (send_instr, chan_idx, value) ->
      let chan = t.chan_table_info.(chan_idx).src in
      let chan_layout = Ir.DType.layout chan.d.dtype in
      let value =
        Ir.DType.value_of_cint_exn chan.d.dtype value
        |> Ir.DType.sexp_of_t_ chan.d.dtype
      in
      Error
        [%string
          "Sent value doesnt fit in chan: got %{value#Sexp} but channel has \
           layout %{Ir.Layout.sexp_of_t chan_layout#Sexp} at %{str_i \
           send_instr}."]
  | `Written_mem_value_doesnt_fit_in_cell (write_instr, mem_idx, value) ->
      let mem = t.mem_table_info.(mem_idx).src in
      let mem_cell_layout = Ir.DType.layout mem.d.dtype in
      let value =
        Ir.DType.value_of_cint_exn mem.d.dtype value
        |> Ir.DType.sexp_of_t_ mem.d.dtype
      in
      Error
        [%string
          "Written value doesnt fit in memory cell: got %{value#Sexp} but \
           memory cell has layout %{Ir.Layout.sexp_of_t mem_cell_layout#Sexp} \
           at %{str_i write_instr}."]
  | `Eval_expr_failed (expr_src, pc, error_string) ->
      let expr_src =
        match expr_src with
        | `Assert -> "assert statement"
        | `Assign -> "assign statement"
        | `Guard idx -> [%string "guard of index %{idx#Int}"]
        | `Jump_if_false -> "while loop guard"
        | `Log1 -> "log statement"
        | `Mem_idx -> "index into memeory"
        | `Send -> "channel send statement"
        | `Write_mem_value -> "value being written to memory"
      in
      Error
        [%string
          "Error while evaluating expression from %{expr_src} at %{str_i pc}: \
           %{error_string}."]
  | `Unstable_probe (pc, probe) -> (
      match probe with
      | Probe.Read_ready chan_idx ->
          let chan_creation_pos =
            t.chan_table_info.(chan_idx).src.d.creation_code_pos
          in
          Error
            [%string
              "Unstable waiting %{str_i pc} for send-ready for chan created \
               %{str_l chan_creation_pos}."]
      | Send_ready chan_idx ->
          let chan_creation_pos =
            t.chan_table_info.(chan_idx).src.d.creation_code_pos
          in
          Error
            [%string
              "Unstable waiting %{str_i pc} for send-ready for chan created \
               %{str_l chan_creation_pos}"])
  | `Select_multiple_true_probes (pc, true_probes) ->
      let branch_idxs = List.map true_probes ~f:fst in
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true probes: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]

let wait_ t ~max_steps () =
  let queued_user_ops = Queue.to_list t.queued_user_ops in
  Queue.clear t.queued_user_ops;
  let to_send, to_read =
    List.partition_map queued_user_ops
      ~f:(fun { queuer; chan_instr; value; call_site } ->
        match queuer with
        | `Send enqueuer -> First (chan_instr, (enqueuer, value, call_site))
        | `Read dequeuer -> Second (chan_instr, (dequeuer, value, call_site)))
  in
  Int.Map.of_alist_multi to_send
  |> Map.iteri ~f:(fun ~key:send_instr ~data:l ->
         let enqueuer_idx, _, _ = List.hd_exn l in
         let values =
           List.map l ~f:(fun (_, value, origin) ->
               { With_origin.value; origin })
         in
         let values = Array.of_list values in
         Inner_data.set_enqueuer t.i ~enqueuer_idx ~is_done:false ~idx:0
           ~to_send:values ~push_pc:(send_instr + 1));
  Int.Map.of_alist_multi to_read
  |> Map.iteri ~f:(fun ~key:read_instr ~data:l ->
         let dequeuer_idx, _, _ = List.hd_exn l in
         let values =
           List.map l ~f:(fun (_, value, origin) ->
               { With_origin.value; origin })
         in
         let values = Array.of_list values in
         Inner_data.set_dequeuer t.i ~dequeuer_idx ~idx:0 ~expected_reads:values
           ~push_pc:read_instr);
  Inner_data.wait t.i ~max_steps ()

let wait t ?(max_steps = 1000) ?(line_numbers = true) () =
  let status =
    match t.is_done with
    | true -> `Already_done
    | false -> wait_ t ~max_steps ()
  in
  let status =
    resolve_step_err t status ~line_numbers
    |> Result.map_error ~f:Error.of_string
  in
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
    id_of_var : Var_id.t Ir.Var.U.Table.t;
    src_of_id : (Any.t Ir.DType.t option * Var_id_src.t) Var_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      id_of_var = Ir.Var.U.Table.create ();
      src_of_id = Var_id.Table.create ();
    }

  let new_id t src dtype =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.set t.src_of_id ~key:id ~data:(dtype, src);
    id

  let to_assem_id t var =
    Hashtbl.find_or_add t.id_of_var var ~default:(fun () ->
        new_id t (Var_id_src.Var var) (Some var.d.dtype))
end

module Chan_id_pool = struct
  type t = { mutable next_id : int; id_of_chan : Chan_id.t Ir.Chan.U.Table.t }

  let create () = { next_id = 0; id_of_chan = Ir.Chan.U.Table.create () }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t chan =
    Hashtbl.find_or_add t.id_of_chan chan ~default:(fun () -> new_id t)
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
          Var_id_pool.new_id var_id_pool Mem_idx_reg None
        in
        (helper_reg_var_idx, mem_idx))
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
    let rec convert x =
      let imap2 a b (f : CInt.t -> CInt.t -> CInt.t) =
        Expr.map2 (convert a) (convert b) ~f
      in
      match x with
      | Ir.Expr.K.Var var_id -> Expr.Var (convert_id var_id)
      | Const c -> Const c
      | Add (a, b) -> imap2 a b CInt.( + )
      | Sub_no_wrap (a, b) ->
          let a, b = (convert a, convert b) in
          Expr.assert_then_or_log2
            (Expr.map2 a b ~f:(fun a b ->
                 CInt.( >= ) a b |> Bool.to_int |> CInt.of_int))
            (Expr.map2 a b ~f:CInt.( - ))
            a b
            (fun a b ->
              [%string
                "Expr.Sub_no_wrap must have first arg (%{a#CInt}) >= second \
                 arg (%{b#CInt})"])
      | Sub_wrap (a, b, bits) -> imap2 a b (CInt.sub_wrap ~bits)
      | Mul (a, b) -> imap2 a b CInt.( * )
      | Div (a, b) -> imap2 a b CInt.( / )
      | Mod (a, b) -> imap2 a b CInt.( % )
      | LShift (a, b) -> imap2 a b CInt.shift_left
      | LogicalRShift (a, b) -> imap2 a b CInt.shift_right_logical
      | BitAnd (a, b) -> imap2 a b CInt.bit_and
      | BitOr (a, b) -> imap2 a b CInt.bit_or
      | BitXor (a, b) -> imap2 a b CInt.bit_xor
      | Eq (a, b) ->
          imap2 a b (fun a b -> CInt.equal a b |> Bool.to_int |> CInt.of_int)
      | Ne (a, b) ->
          imap2 a b (fun a b ->
              (not (CInt.equal a b)) |> Bool.to_int |> CInt.of_int)
      | Lt (a, b) ->
          imap2 a b (fun a b -> CInt.( < ) a b |> Bool.to_int |> CInt.of_int)
      | Le (a, b) ->
          imap2 a b (fun a b -> CInt.( <= ) a b |> Bool.to_int |> CInt.of_int)
      | Gt (a, b) ->
          imap2 a b (fun a b -> CInt.( > ) a b |> Bool.to_int |> CInt.of_int)
      | Ge (a, b) ->
          imap2 a b (fun a b -> CInt.( >= ) a b |> Bool.to_int |> CInt.of_int)
      | Clip (a, bits) -> Expr.map (convert a) ~f:(CInt.clip ~bits)
      | With_assert_log (assert_expr, val_expr, log_input, msg_fn) ->
          Expr.assert_then_or_log1 (convert assert_expr) (convert val_expr)
            (convert log_input) (fun v -> msg_fn v)
      | With_assert_log_fn (_, _, _) -> failwith "TODO"
    in
    convert (Ir.Expr.untype expr).k
  in

  let chan_id_pool = Chan_id_pool.create () in
  let get_chan chan_id = Chan_id_pool.get_id chan_id_pool chan_id in

  let mem_id_pool = Mem_id_pool.create () in
  let get_mem mem = Mem_id_pool.get_id mem_id_pool var_id_pool mem in

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
    let push_select_probes loc branches =
      let split, starts, merge =
        let split = push_instr loc Nop in
        let starts, ends =
          List.map branches ~f:(fun (probe, stmt) ->
              let other_probes =
                List.map branches ~f:fst
                |> List.filter ~f:(fun o -> not (Probe.equal o probe))
              in
              let start =
                push_instr loc (SelectProbes_AssertStable (probe, other_probes))
              in
              convert' stmt;
              let end_ = push_instr loc (Jump Instr_idx.dummy_val) in
              (start, end_))
          |> List.unzip
        in
        let merge = push_instr loc Nop in
        List.iter ends ~f:(fun end_ -> edit_instr loc end_ (Jump merge));
        (split, starts, merge)
      in
      let guards =
        List.zip_exn branches starts
        |> List.map ~f:(fun ((probe, _), start) -> (probe, start))
      in
      edit_instr loc split (SelectProbes guards);
      merge
    in
    match stmt with
    | Ir.N.Assign (loc, id, expr) ->
        push_instr loc (Assign (convert_id id, convert_expr expr))
    | Nop -> push_instr Code_pos.dummy_loc Nop
    | Log (loc, str) -> push_instr loc (Log0 str)
    | Log1 (loc, expr, f) ->
        push_instr loc
          (Log1
             ( convert_expr expr,
               fun i ->
                 Ir.Expr.Tag.value_of_cint expr.tag i |> Option.value_exn |> f
             ))
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
        push_select_probes loc [ (Read_ready chan_idx, Nop) ]
    | WaitUntilSendReady (loc, chan) ->
        let chan_idx = get_chan chan in
        push_select_probes loc [ (Send_ready chan_idx, Nop) ]
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
  let all_dequeuers, dequeuer_table =
    Set.to_list user_readable_ports
    |> List.mapi ~f:(fun dequeuer_idx chan ->
           let chan_idx = Chan_id_pool.get_id chan_id_pool chan in
           let var_id =
             Var_id_pool.new_id var_id_pool Read_deq_reg (Some chan.d.dtype)
           in
           let read_instr =
             push_instr Code_pos.dummy_loc (Read (var_id, chan_idx))
           in
           let _ = push_instr Code_pos.dummy_loc (Read_dequeuer dequeuer_idx) in
           (* let dequeuer = Dequeuer_buff.create ~var_id chan_idx in *)
           ((chan, (read_instr, dequeuer_idx)), (var_id, chan_idx)))
    |> List.unzip
  in
  let all_dequeuers = Ir.Chan.U.Map.of_alist_exn all_dequeuers in
  let dequeuer_table, dequeuer_table_info =
    List.map dequeuer_table ~f:(fun (var_id, chan) ->
        (Dequeuer_buff.create ~var_id, { Dequeuer_info.chan }))
    |> Array.of_list |> Array.unzip
  in

  (* set up user dequeuers *)
  let all_enqueuers, enqueuer_table =
    Set.to_list user_sendable_ports
    |> List.mapi ~f:(fun enqueuer_idx chan ->
           let chan_idx = Chan_id_pool.get_id chan_id_pool chan in
           let var_id =
             Var_id_pool.new_id var_id_pool Send_enq_reg (Some chan.d.dtype)
           in
           let send_instr =
             push_instr Code_pos.dummy_loc (Send (Var var_id, chan_idx))
           in
           let _ = push_instr Code_pos.dummy_loc (Send_enqueuer enqueuer_idx) in
           (* let enqueuer =  in *)
           ((chan, (send_instr, enqueuer_idx)), (var_id, chan_idx)))
    |> List.unzip
  in
  let all_enqueuers = Ir.Chan.U.Map.of_alist_exn all_enqueuers in
  let enqueuer_table, enqueuer_table_info =
    List.map enqueuer_table ~f:(fun (var_id, chan) ->
        (Enqueuer_buff.create ~var_id, { Enqueuer_info.chan }))
    |> Array.of_list |> Array.unzip
  in

  let assem = Assem_builder.assem_array ab in
  let assem, loc_of_assem_idx = Array.unzip assem in
  let var_table, var_table_info =
    let var_inits =
      Hashtbl.to_alist var_id_pool.src_of_id
      |> List.sort ~compare:(fun (id1, _) (id2, _) -> Int.compare id1 id2)
    in
    List.iteri var_inits ~f:(fun i (id, _) -> assert (Int.equal i id));
    List.map var_inits ~f:(fun (_, (dtype, src)) ->
        let value, is_inited =
          match src with
          | Var_id_src.Mem_idx_reg | Read_deq_reg | Send_enq_reg ->
              (CInt.zero, false)
          | Var var -> (
              match var.Ir.Var.U.d.init with
              | Some value ->
                  let value = Ir.DType.cint_of_value var.d.dtype value in
                  (value, true)
              | None -> (CInt.zero, false))
        in
        let bitwidth =
          Option.map dtype ~f:(fun dtype ->
              match Ir.DType.layout dtype with Bits_fixed bitwidth -> bitwidth)
          |> Option.value ~default:0
        in
        let dtype = Option.value dtype ~default:Ir.DType.dummy_val in
        let buff =
          { Var_buff.bitwidth; value; is_inited; read_ct = 0; write_ct = 0 }
        in
        let info = { Var_buff_info.src; dtype } in
        (buff, info))
    |> Array.of_list |> Array.unzip
  in
  let pcs = Vec.of_array [| 0 |] ~default:(-1) in
  let rng = Random.State.make [| seed |] in

  let chan_table, chan_table_info =
    let tbl =
      Hashtbl.to_alist chan_id_pool.id_of_chan
      |> List.map ~f:(fun (chan, chan_id) -> (chan_id, chan))
      |> List.sort ~compare:(fun (id0, _) (id1, _) -> Int.compare id0 id1)
    in
    List.iteri tbl ~f:(fun i (chan_id, _) -> assert (Int.equal i chan_id));
    List.map tbl ~f:(fun (_, chan) ->
        let bitwidth =
          match Ir.DType.layout chan.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        let buff = Chan_buff.create bitwidth in
        let info = { Chan_buff_info.src = chan } in
        (buff, info))
    |> Array.of_list |> Array.unzip
  in
  let mem_table, mem_table_info =
    let tbl =
      Hashtbl.to_alist mem_id_pool.id_of_mem
      |> List.map ~f:(fun (mem, (idx_reg, mem_id)) -> (mem_id, (idx_reg, mem)))
      |> List.sort ~compare:(fun (id0, _) (id1, _) -> Int.compare id0 id1)
    in
    List.iteri tbl ~f:(fun i (mem_id, _) -> assert (Int.equal i mem_id));
    List.map tbl ~f:snd
    |> List.map ~f:(fun (idx_helper_reg, mem) ->
           let buff = Mem_buff.create ~idx_helper_reg mem in
           let info = { Mem_buff_info.src = mem } in
           (buff, info))
    |> Array.of_list |> Array.unzip
  in
  let i =
    {
      Inner_data.assem;
      pcs;
      var_table;
      chan_table;
      enqueuer_table;
      dequeuer_table;
      mem_table;
      rng;
    }
  in
  {
    i;
    is_done = false;
    loc_of_assem_idx;
    var_table_info;
    chan_table_info;
    enqueuer_table_info;
    dequeuer_table_info;
    mem_table_info;
    all_enqueuers;
    all_dequeuers;
    queued_user_ops = Queue.create ();
  }

let queue_user_io_op t call_site chan value chan_instr queuer =
  let value = Any.of_magic value in
  let ivalue = Ir.DType.cint_of_value chan.Ir.Chan.U.d.dtype value in
  let chan_bitwidth =
    match Ir.DType.layout chan.d.dtype with Bits_fixed bitwidth -> bitwidth
  in
  if chan_bitwidth >= CInt.bitwidth ivalue then
    Queue.enqueue t.queued_user_ops
      { Queued_user_op.queuer; chan_instr; value = ivalue; call_site }
  else
    let value = Ir.DType.sexp_of_t_ chan.d.dtype value in
    let layout = Ir.DType.layout chan.d.dtype |> Ir.Layout.sexp_of_t in
    failwith
      [%string
        "Value doesnt fit in chan: got value %{value#Sexp} but channel has \
         layout %{layout#Sexp}."]

let send t ?loc chan value =
  let chan = Ir.Chan.unwrap_w chan in
  let call_site = Code_pos.value_or_psite loc in
  match Map.find t.all_enqueuers chan with
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-sendable chan in \
         Sim.create"
  | Some (send_instr, enqueuer) ->
      queue_user_io_op t call_site chan value send_instr (`Send enqueuer)

let read t ?loc chan value =
  let chan = Ir.Chan.unwrap_r chan in
  let call_site = Code_pos.value_or_psite loc in
  match Map.find t.all_dequeuers chan with
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-readable chan in \
         Sim.create"
  | Some (read_instr, dequeuer) ->
      queue_user_io_op t call_site chan value read_instr (`Read dequeuer)
