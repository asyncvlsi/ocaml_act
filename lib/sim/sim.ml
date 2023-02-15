open! Core
open! Act
module Ir = Internal_rep

module With_origin = struct
  type 'a t = { value : 'a; origin : Code_pos.t } [@@deriving sexp_of, fields]

  let map { value; origin } ~f = { value = f value; origin }
end

module Var_id_src = struct
  type t = Var of Ir.Var.U.t | Mem_idx_reg | Read_deq_reg | Send_enq_reg
  [@@deriving sexp_of]
end

module Var_buff_info = struct
  type t = { src : Var_id_src.t; dtype : Any.t Ir.DType.t }
end

module Chan_buff_info = struct
  type t = { src : (Ir.Chan.U.t[@sexp.opaque]) } [@@deriving sexp_of]
end

module Mem_buff_info = struct
  type t = { src : (Ir.Mem.t[@sexp.opaque]) } [@@deriving sexp_of]
end

module Enqueuer_info = struct
  type t = { chan : Inner.Chan_id.t } [@@deriving sexp_of]
end

module Dequeuer_info = struct
  type t = { chan : Inner.Chan_id.t } [@@deriving sexp_of]
end

module Queued_user_op = struct
  type t = {
    queuer : [ `Send of Inner.Enqueuer_idx.t | `Read of Inner.Dequeuer_idx.t ];
    chan_instr : Inner.Instr_idx.t;
    value : CInt.t;
    call_site : Code_pos.t;
  }
end

type t = {
  i : Inner.t;
  mutable is_done : bool;
  (* error message helpers *)
  loc_of_assem_idx : Code_pos.t array;
  var_table_info : Var_buff_info.t array;
  chan_table_info : Chan_buff_info.t array;
  mem_table_info : Mem_buff_info.t array;
  enqueuer_table_info : Enqueuer_info.t array;
  dequeuer_table_info : Dequeuer_info.t array;
  expr_assert_error_decoders : (CInt.t -> CInt.t -> string) array;
  (* io helpers *)
  all_enqueuers : (Inner.Instr_idx.t * Inner.Enqueuer_idx.t) Ir.Chan.U.Map.t;
  all_dequeuers : (Inner.Instr_idx.t * Inner.Dequeuer_idx.t) Ir.Chan.U.Map.t;
  (* per-wait state *)
  queued_user_ops : Queued_user_op.t Queue.t;
}

let resolve_step_err t e ~line_numbers ~to_send ~to_read =
  (* Now this is a map of form Enquere_idx.t -> CInt.t With_origin.t *)
  let to_send = Map.data to_send |> Int.Map.of_alist_exn in
  let to_read = Map.data to_read |> Int.Map.of_alist_exn in
  let loc_of_instr var_id = t.loc_of_assem_idx.(var_id) in
  let str_l (cp : Code_pos.t) =
    if line_numbers then
      [%string "in %{cp.filename} on line %{cp.line_number#Int}"]
    else "<loc>"
  in
  let str_i pc = loc_of_instr pc |> str_l in
  match e with
  | Inner.E.Stuck -> Ok `Stuck
  | Time_out -> Error "Simulation timed out. Maybe increase max_steps?"
  | User_read_did_not_complete (deq_idx, read_idx) ->
      let chan_idx = t.dequeuer_table_info.(deq_idx).chan in
      let chan_creation_pos =
        t.chan_table_info.(chan_idx).src.d.creation_code_pos
      in
      let read = (Map.find_exn to_read deq_idx).(read_idx) in
      Error
        [%string
          "User read did not complete:  called %{str_l \
           read.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | User_send_did_not_complete (enq_idx, send_idx) ->
      let chan_idx = t.enqueuer_table_info.(enq_idx).chan in
      let chan_creation_pos =
        t.chan_table_info.(chan_idx).src.d.creation_code_pos
      in
      let send = (Map.find_exn to_send enq_idx).(send_idx - 1) in
      Error
        [%string
          "User send did not complete:  called %{str_l \
           send.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | Uninit_id (var_id, pc) ->
      let var_code_pos =
        match t.var_table_info.(var_id).src with
        | Var var ->
            (* str_l var.d.creation_code_pos *)
            Ir.Var.U.Id.to_string var.id
        | Mem_idx_reg | Read_deq_reg | Send_enq_reg -> failwith "unreachable"
      in
      Error
        [%string
          "Uninitialized variable: read %{str_i pc}, created %{var_code_pos}."]
  | Simul_chan_senders (fst_pc, snd_pc) ->
      Error
        [%string
          "Simultanious senders on channel: statement 1 %{str_i fst_pc}, \
           statement 2 %{str_i snd_pc}."]
  | Simul_chan_readers (fst_pc, snd_pc) ->
      Error
        [%string
          "Simultanious readers on channel: statement 1 %{str_i fst_pc}, \
           statement 2 %{str_i snd_pc}."]
  | Assert_failure pc -> Error [%string "Assertion failed: %{str_i pc}."]
  | Simul_read_write_var (read_pc, write_pc, var_id) ->
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
  | Simul_write_write_var (pc1, pc2, var_id) -> (
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
  | Select_no_guards_true pc ->
      Error [%string "Select statement has no true guards: %{str_i pc}."]
  | Select_multiple_guards_true (pc, branch_idxs) ->
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true guards: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]
  | Read_dequeuer_wrong_value (deq_idx, actual, expected_idx) ->
      let chan_idx = t.dequeuer_table_info.(deq_idx).chan in
      let chan_dtype = t.chan_table_info.(chan_idx).src.d.dtype in
      let actual =
        Ir.DType.value_of_cint_exn chan_dtype actual
        |> Ir.DType.sexp_of_t_ chan_dtype
      in
      let expected = (Map.find_exn to_read deq_idx).(expected_idx) in
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
  | Mem_out_of_bounds (pc, idx, len) ->
      Error
        [%string
          "Mem access out of bounds: %{str_i pc}, idx is %{idx#CInt}, size of \
           mem is %{len#Int}."]
  | Assigned_value_doesnt_fit_in_var (assign_instr, var_id, value) ->
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
  | Read_chan_value_doesnt_fit_in_var (read_instr, read_dst_var_id, value) ->
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
  | Read_mem_value_doesnt_fit_in_var (read_instr, dst_id, value) ->
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
  | Sent_value_doesnt_fit_in_chan (send_instr, chan_idx, value) ->
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
  | Written_mem_value_doesnt_fit_in_cell (write_instr, mem_idx, value) ->
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
  | Eval_expr_failed (expr_kind, expr_err_id, cint0, cint1, pc) ->
      let expr_kind =
        match expr_kind with
        | Assert -> "assert statement"
        | Assign -> "assign statement"
        | Guard idx -> [%string "guard of index %{idx#Int}"]
        | Jump_if_false -> "while loop guard"
        | Log1 -> "log statement"
        | Mem_idx -> "index into memeory"
        | Send -> "channel send statement"
        | Write_mem_value -> "value being written to memory"
      in
      let error_string =
        let f = t.expr_assert_error_decoders.(expr_err_id) in
        f cint0 cint1
      in
      Error
        [%string
          "Error while evaluating expression from %{expr_kind} at %{str_i pc}: \
           %{error_string}."]
  | Unstable_probe (pc, probe) -> (
      match probe with
      | Inner.Probe.Read_ready chan_idx ->
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
  | Select_multiple_true_probes (pc, true_probes) ->
      let branch_idxs = List.map true_probes ~f:fst in
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true probes: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]

let wait t ?(max_steps = 1000) ?(line_numbers = true) () =
  let queued_user_ops = Queue.to_list t.queued_user_ops in
  Queue.clear t.queued_user_ops;
  let to_send, to_read =
    List.partition_map queued_user_ops
      ~f:(fun { queuer; chan_instr; value; call_site } ->
        match queuer with
        | `Send enqueuer -> First (chan_instr, (enqueuer, value, call_site))
        | `Read dequeuer -> Second (chan_instr, (dequeuer, value, call_site)))
  in
  let to_send =
    Int.Map.of_alist_multi to_send
    |> Map.map ~f:(fun l ->
           let enqueuer_idx, _, _ = List.hd_exn l in
           let values =
             List.map l ~f:(fun (_, value, origin) ->
                 { With_origin.value; origin })
           in
           let values = Array.of_list values in
           (enqueuer_idx, values))
  in
  let to_read =
    Int.Map.of_alist_multi to_read
    |> Map.map ~f:(fun l ->
           let dequeuer_idx, _, _ = List.hd_exn l in
           let values =
             List.map l ~f:(fun (_, value, origin) ->
                 { With_origin.value; origin })
           in
           let values = Array.of_list values in
           (dequeuer_idx, values))
  in
  match t.is_done with
  | true -> Error (Error.of_string "Already done.")
  | false ->
      let status =
        Map.iteri to_send
          ~f:(fun ~key:send_instr ~data:(enqueuer_idx, to_send) ->
            let to_send = Array.map to_send ~f:With_origin.value in
            Inner.set_enqueuer t.i ~enqueuer_idx ~is_done:false ~idx:0 ~to_send
              ~push_pc:(send_instr + 1));
        Map.iteri to_read
          ~f:(fun ~key:read_instr ~data:(dequeuer_idx, expected_reads) ->
            let expected_reads =
              Array.map expected_reads ~f:With_origin.value
            in
            Inner.set_dequeuer t.i ~dequeuer_idx ~idx:0 ~expected_reads
              ~push_pc:read_instr);
        Inner.wait t.i ~max_steps ()
        |> resolve_step_err t ~line_numbers ~to_send ~to_read
        |> Result.map_error ~f:Error.of_string
      in
      Result.iter_error status ~f:(fun _ -> t.is_done <- true);
      Result.map status ~f:(fun `Stuck -> ())

let wait' t ?max_steps () =
  print_s [%sexp (wait t ?max_steps () : unit Or_error.t)]

module Assem_builder = struct
  type t = { assem : (Inner.N.t * Code_pos.t) Vec.t }

  let create () =
    { assem = Vec.create ~cap:10 ~default:(Inner.N.End, Code_pos.dummy_loc) }

  let push t loc (instr : Inner.N.t) =
    Vec.push t.assem (instr, loc);
    Vec.length t.assem - 1

  let edit t loc idx (instr : Inner.N.t) = Vec.set t.assem idx (instr, loc)
  let next_idx t = Vec.length t.assem
  let assem_array t = Vec.to_array t.assem
end

module Var_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_var : Inner.Var_id.t Ir.Var.U.Table.t;
    src_of_id : (Any.t Ir.DType.t option * Var_id_src.t) Inner.Var_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      id_of_var = Ir.Var.U.Table.create ();
      src_of_id = Inner.Var_id.Table.create ();
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
  type t = {
    mutable next_id : int;
    id_of_chan : Inner.Chan_id.t Ir.Chan.U.Table.t;
  }

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
    id_of_mem : (Inner.Var_id.t * Inner.Mem_id.t) Ir.Mem.Table.t;
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

let create_t ~seed ir ~user_sendable_ports ~user_readable_ports =
  let ir = Ir.Chp.unwrap ir in
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

  let expr_assert_error_decoders =
    Vec.create ~cap:10 ~default:(fun _ _ -> "")
  in
  let add_expr_assert ~f =
    Vec.push expr_assert_error_decoders (fun a b -> f a b);
    Vec.length expr_assert_error_decoders - 1
  in

  let var_id_pool = Var_id_pool.create () in
  let convert_id id = Var_id_pool.to_assem_id var_id_pool id in
  (* Turns an Ir.Expr into a Inner.Expr. Inner.Expr is a flat array. This code
     dedupicates repeated nodes. *)
  let convert_expr expr =
    let ns = Vec.create ~cap:10 ~default:(Inner.Expr.N.Const CInt.zero) in
    let ni_of_n = Inner.Expr.N.Table.create () in
    let push n =
      Hashtbl.find_or_add ni_of_n n ~default:(fun () ->
          let n =
            match n with Assert _ -> failwith "use push_assert" | _ -> n
          in
          Vec.push ns n;
          Vec.length ns - 1)
    in
    let push' n = ignore (push n : Inner.Expr.NI.t) in
    let next_assert_id = ref 0 in
    let asserts = Vec.create ~cap:10 ~default:(0, 0, 0) in
    let push_assert a err_no d1 d2 =
      Hashtbl.find_or_add ni_of_n
        (Assert (a, 0))
        ~default:(fun () ->
          let id = !next_assert_id in
          incr next_assert_id;
          Vec.push ns (Assert (a, id));
          Vec.push asserts (err_no, d1, d2);
          Vec.length ns - 1)
      |> fun ni -> ignore (ni : Inner.Expr.NI.t)
    in
    let rec convert x =
      match x with
      | Ir.Expr.K.Var var_id -> push (Var (convert_id var_id))
      | Const c -> push (Const c)
      | Add (a, b) -> push (Add (convert a, convert b))
      | Sub_no_wrap (a, b) ->
          let a, b = (convert a, convert b) in
          let err_id =
            add_expr_assert ~f:(fun a b ->
                [%string
                  "Expr.Sub_no_wrap must have first arg (%{a#CInt}) >= second \
                   arg (%{b#CInt})"])
          in
          let assert_expr = push (Ge (a, b)) in
          push_assert assert_expr err_id a b;
          push (Sub_no_underflow (a, b))
      | Sub_wrap (a, b, bits) ->
          let a, b = (convert a, convert b) in
          let p2bits =
            push (Const (CInt.shift_left CInt.one (CInt.of_int bits)))
          in
          let a = push (Clip (a, bits)) in
          let a = push (BitOr (a, p2bits)) in
          let b = push (Clip (b, bits)) in
          let diff = push (Sub_no_underflow (a, b)) in
          push (Clip (diff, bits))
      | Mul (a, b) -> push (Mul (convert a, convert b))
      | Div (a, b) -> push (Div (convert a, convert b))
      | Mod (a, b) -> push (Mod (convert a, convert b))
      | LShift (a, b) -> push (LShift (convert a, convert b))
      | LogicalRShift (a, b) -> push (RShift (convert a, convert b))
      | BitAnd (a, b) -> push (BitAnd (convert a, convert b))
      | BitOr (a, b) -> push (BitOr (convert a, convert b))
      | BitXor (a, b) -> push (BitXor (convert a, convert b))
      | Eq (a, b) -> push (Eq (convert a, convert b))
      | Ne (a, b) -> push (Ne (convert a, convert b))
      | Lt (a, b) -> push (Lt (convert a, convert b))
      | Le (a, b) -> push (Le (convert a, convert b))
      | Gt (a, b) -> push (Gt (convert a, convert b))
      | Ge (a, b) -> push (Ge (convert a, convert b))
      | Clip (a, bits) -> push (Clip (convert a, bits))
      | With_assert_log (assert_expr, val_expr, log_input, msg_fn) ->
          let err_id = add_expr_assert ~f:(fun v _ -> msg_fn v) in
          let assert_expr = convert assert_expr in
          let log_input = convert log_input in
          let c0 = push (Const CInt.zero) in
          push_assert assert_expr err_id log_input c0;
          convert val_expr
    in
    let e = convert (Ir.Expr.untype expr).k in
    push' (Return e);
    { Inner.Expr.ns = Vec.to_array ns; asserts = Vec.to_array asserts }
  in
  let chan_id_pool = Chan_id_pool.create () in
  let get_chan chan_id = Chan_id_pool.get_id chan_id_pool chan_id in

  let mem_id_pool = Mem_id_pool.create () in
  let get_mem mem = Mem_id_pool.get_id mem_id_pool var_id_pool mem in

  let rec convert_stmt stmt =
    let convert' stmt = ignore (convert_stmt stmt : Inner.Instr_idx.t) in
    let push_branches loc stmts =
      let split = push_instr loc Nop in
      let ends =
        List.map stmts ~f:(fun stmt ->
            convert' stmt;
            push_instr loc (Jump Inner.Instr_idx.dummy_val))
      in
      let starts =
        List.take (split :: ends) (List.length stmts)
        |> List.map ~f:Inner.Instr_idx.next
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
                |> List.filter ~f:(fun o -> not (Inner.Probe.equal o probe))
              in
              let start =
                push_instr loc (SelectProbes_AssertStable (probe, other_probes))
              in
              convert' stmt;
              let end_ = push_instr loc (Jump Inner.Instr_idx.dummy_val) in
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
    | Ir.Chp.Assign (loc, id, expr) ->
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
          (ParJoin (Inner.Par_join.create ~max_ct:(List.length stmts)));
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
          push_instr loc
            (JumpIfFalse (convert_expr expr, Inner.Instr_idx.dummy_val))
        in
        convert' seq;
        let jmp = push_instr loc (Jump split) in
        edit_instr loc split
          (JumpIfFalse (convert_expr expr, Inner.Instr_idx.next jmp));
        jmp
    | DoWhile (loc, seq, expr) ->
        let top = push_instr loc Nop in
        convert' seq;
        let not_expr = Ir.Expr.wrap expr |> CBool.E.not_ |> Ir.Expr.unwrap in
        push_instr loc (JumpIfFalse (convert_expr not_expr, top))
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
    let init_jump =
      push_instr Code_pos.dummy_loc (Jump Inner.Instr_idx.dummy_val)
    in
    let start = Assem_builder.next_idx ab in
    edit_instr Code_pos.dummy_loc init_jump (Jump start)
  in
  let (_ : Inner.Instr_idx.t) = convert_stmt ir in
  let (_ : Inner.Instr_idx.t) = push_instr Code_pos.dummy_loc End in

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
  let dequeuers, dequeuer_table_info =
    List.map dequeuer_table ~f:(fun (var_id, chan) ->
        ({ Inner.Dequeuer_spec.var_id }, { Dequeuer_info.chan }))
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
           let send_expr =
             { Inner.Expr.ns = [| Var var_id; Return 0 |]; asserts = [||] }
           in
           let send_instr =
             push_instr Code_pos.dummy_loc (Send (send_expr, chan_idx))
           in
           let _ = push_instr Code_pos.dummy_loc (Send_enqueuer enqueuer_idx) in
           (* let enqueuer =  in *)
           ((chan, (send_instr, enqueuer_idx)), (var_id, chan_idx)))
    |> List.unzip
  in
  let all_enqueuers = Ir.Chan.U.Map.of_alist_exn all_enqueuers in
  let enqueuers, enqueuer_table_info =
    List.map enqueuer_table ~f:(fun (var_id, chan) ->
        ({ Inner.Enqueuer_spec.var_id }, { Enqueuer_info.chan }))
    |> Array.of_list |> Array.unzip
  in

  let assem = Assem_builder.assem_array ab in
  let assem, loc_of_assem_idx = Array.unzip assem in
  let assem_guard_read_ids =
    Array.map assem ~f:(fun n -> Inner.N.get_read_ids n)
  in
  let assem_guard_write_ids =
    Array.map assem ~f:(fun n -> Inner.N.get_write_ids n)
  in
  let vars, var_table_info =
    let var_srcs =
      Hashtbl.to_alist var_id_pool.src_of_id
      |> List.sort ~compare:(fun (id1, _) (id2, _) -> Int.compare id1 id2)
    in
    List.iteri var_srcs ~f:(fun i (id, _) -> assert (Int.equal i id));
    List.map var_srcs ~f:(fun (_, (dtype, src)) ->
        let bitwidth =
          Option.map dtype ~f:(fun dtype ->
              match Ir.DType.layout dtype with Bits_fixed bitwidth -> bitwidth)
          |> Option.value ~default:0
        in
        let dtype = Option.value dtype ~default:Ir.DType.dummy_val in
        let init =
          match src with
          | Var_id_src.Mem_idx_reg | Read_deq_reg | Send_enq_reg -> None
          | Var var ->
              var.Ir.Var.U.d.init
              |> Option.map ~f:(fun init ->
                     Ir.DType.cint_of_value var.d.dtype init)
        in
        let spec = { Inner.Var_spec.bitwidth; init } in
        let info = { Var_buff_info.src; dtype } in
        (spec, info))
    |> Array.of_list |> Array.unzip
  in

  let chans, chan_table_info =
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
        let spec = { Inner.Chan_spec.bitwidth } in
        let info = { Chan_buff_info.src = chan } in
        (spec, info))
    |> Array.of_list |> Array.unzip
  in
  let mems, mem_table_info =
    let tbl =
      Hashtbl.to_alist mem_id_pool.id_of_mem
      |> List.map ~f:(fun (mem, (idx_reg, mem_id)) -> (mem_id, (idx_reg, mem)))
      |> List.sort ~compare:(fun (id0, _) (id1, _) -> Int.compare id0 id1)
    in
    List.iteri tbl ~f:(fun i (mem_id, _) -> assert (Int.equal i mem_id));
    List.map tbl ~f:(fun (_, (idx_helper_reg, mem)) ->
        let init =
          Array.map mem.d.init ~f:(fun v ->
              Ir.DType.cint_of_value mem.d.dtype v)
        in
        let cell_bitwidth =
          match Ir.DType.layout mem.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        let spec = { Inner.Mem_spec.cell_bitwidth; idx_helper_reg; init } in
        let info = { Mem_buff_info.src = mem } in
        (spec, info))
    |> Array.of_list |> Array.unzip
  in
  let i =
    Inner.create ~assem ~assem_guard_read_ids ~assem_guard_write_ids ~vars
      ~chans ~enqueuers ~dequeuers ~mems ~seed
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
    expr_assert_error_decoders = Vec.to_array expr_assert_error_decoders;
    all_enqueuers;
    all_dequeuers;
    queued_user_ops = Queue.create ();
  }

let reset t =
  Inner.reset t.i;
  t.is_done <- false;
  Queue.clear t.queued_user_ops

let create ?(seed = 0) ir ~user_sendable_ports ~user_readable_ports =
  let t = create_t ~seed ir ~user_sendable_ports ~user_readable_ports in
  reset t;
  t

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
