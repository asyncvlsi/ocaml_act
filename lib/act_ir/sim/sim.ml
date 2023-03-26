open! Core
open Utils
module Mid = Mid

module With_origin = struct
  type 'a t = { value : 'a; origin : Code_pos.t } [@@deriving sexp_of, fields]
end

module Queued_user_op = struct
  type t = {
    queuer : [ `Send of Ir.Chan.t | `Read of Ir.Chan.t ];
    value : CInt.t;
    call_site : Code_pos.t;
  }
  [@@deriving sexp_of]
end

type t = {
  i : Mid.t;
  mutable is_done : bool;
  (* error message helpers *)
  info_of_tag :
    (Code_pos.t * (CInt.t -> Sexp.t) * (CInt.t -> string)) Mid.Tag.Table.t;
  var_of_mid : Ir.Var.t Mid.Var.Map.t;
  chan_of_mid : Ir.Chan.t Mid.Chan.Map.t;
  mem_of_mid : Ir.Mem.t Mid.Mem.Map.t;
  (* io helpers *)
  iports : Mid.Chan.t Ir.Chan.Map.t;
  oports : Mid.Chan.t Ir.Chan.Map.t;
  (* per-wait state *)
  queued_user_ops : Queued_user_op.t Queue.t;
}
[@@deriving sexp_of]

let resolve_step_err t e ~line_numbers ~to_send ~to_read =
  (* Now this is a map of form Enquere_idx.t -> CInt.t With_origin.t *)
  let loc_of_tag tag =
    Hashtbl.find_exn t.info_of_tag tag |> fun (loc, _, _) -> loc
  in
  let sexper_of_tag tag =
    Hashtbl.find_exn t.info_of_tag tag |> fun (_, sexper, _) -> sexper
  in
  let msg_fn_of_tag tag =
    Hashtbl.find_exn t.info_of_tag tag |> fun (_, _, msg_fn) -> msg_fn
  in
  let str_l (cp : Code_pos.t) =
    if line_numbers then
      [%string "in %{cp.filename} on line %{cp.line_number#Int}"]
    else "<loc>"
  in
  let str_i tag = loc_of_tag tag |> str_l in
  let do_sexp tag x = (sexper_of_tag tag) x in
  match e with
  | Mid.E.Stuck -> Ok `Stuck
  | Time_out -> Error "Simulation timed out. Maybe increase max_steps?"
  | User_read_did_not_complete (chan, read_idx) ->
      let chan = Map.find_exn t.chan_of_mid chan in
      let chan_creation_pos = chan.creation_code_pos in
      let read = (Map.find_exn to_read chan).(read_idx) in
      Error
        [%string
          "User read did not complete:  called %{str_l \
           read.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | User_send_did_not_complete (chan, send_idx) ->
      let chan = Map.find_exn t.chan_of_mid chan in
      let chan_creation_pos = chan.creation_code_pos in
      let send = (Map.find_exn to_send chan).(send_idx - 1) in
      Error
        [%string
          "User send did not complete:  called %{str_l \
           send.With_origin.origin}, on chan created %{str_l \
           chan_creation_pos}."]
  | Uninit_id (var, pc) ->
      let var_decl = (Map.find_exn t.var_of_mid var).creation_code_pos in
      Error
        [%string
          "Uninitialized variable: read %{str_i pc}, created %{str_l var_decl}."]
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
  | Assert_failure (pc, cint) ->
      let msg = (msg_fn_of_tag pc) cint in
      Error [%string "Assertion failed %{str_i pc}: %{msg}."]
  | Simul_read_write_var (read_pc, write_pc, var) ->
      let var_decl = (Map.find_exn t.var_of_mid var).creation_code_pos in
      Error
        [%string
          "Simultanious read and write of variable: read %{str_i read_pc}, \
           write %{str_i write_pc}, create %{str_l var_decl}."]
  | Simul_write_write_var (pc1, pc2, var) ->
      let var_decl = (Map.find_exn t.var_of_mid var).creation_code_pos in
      Error
        [%string
          "Simulatnious writes of variable: statement 1 %{str_i pc1}, \
           statement 2 %{str_i pc2}, create %{str_l var_decl}."]
  | Simul_mem_access (pc1, pc2, _mem_id) ->
      Error
        [%string
          "Simulatnious accesses of a memory/rom: statement 1 %{str_i pc1}, \
           statement 2 %{str_i pc2}."]
  | Select_no_guards_true pc ->
      Error [%string "Select statement has no true guards: %{str_i pc}."]
  | Select_multiple_guards_true (pc, branch_idxs) ->
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true guards: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]
  | Read_dequeuer_wrong_value (chan, actual, expected_idx) ->
      let chan = Map.find_exn t.chan_of_mid chan in
      let expected = (Map.find_exn to_read chan).(expected_idx) in
      let chan_decl = chan.creation_code_pos in
      Error
        [%string
          "User read has wrong value: got %{actual#CInt}, but expected \
           %{expected.With_origin.value#CInt} based on `send' function call \
           %{str_l expected.origin}, on chan created %{str_l chan_decl}."]
  | Mem_out_of_bounds (pc, idx, len) ->
      Error
        [%string
          "Mem access out of bounds: %{str_i pc}, idx is %{idx#CInt}, size of \
           mem is %{len#Int}."]
  | Assigned_value_doesnt_fit_in_var (assign_tag, value) ->
      let value = do_sexp assign_tag value in
      Error
        [%string
          "Assigned value doesnt fit in var: got %{value#Sexp} but variable \
           has layout TODO at %{str_i assign_tag}."]
  | Read_chan_value_doesnt_fit_in_var (read_instr, value) ->
      let value = do_sexp read_instr value in
      (* let var_layout = Ir_dtype.layout var_dtype in *)
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout TODO at %{str_i read_instr}."]
  | Read_mem_value_doesnt_fit_in_var (read_instr, value) ->
      let value = do_sexp read_instr value in
      (* let var_dtype = t.var_table_info.(dst_id).dtype in *)
      (* let var_layout = Ir_dtype.layout var_dtype in *)
      (* let value = *)
      (* Ir_dtype.value_of_cint_exn var_dtype value *)
      (* |> Ir_dtype.sexp_of_t_ var_dtype *)
      (* in *)
      Error
        [%string
          "Read value doesnt fit in var: got %{value#Sexp} but variable has \
           layout TODO at %{str_i read_instr}."]
  | Sent_value_doesnt_fit_in_chan (send_instr, value) ->
      let value = do_sexp send_instr value in
      (* let chan = t.chan_table_info.(chan_idx).src in *)
      (* let chan_layout = Ir_dtype.layout chan.d.dtype in *)
      (* let value = *)
      (* Ir_dtype.value_of_cint_exn chan.d.dtype value *)
      (* |> Ir_dtype.sexp_of_t_ chan.d.dtype *)
      (* in *)
      Error
        [%string
          "Sent value doesnt fit in chan: got %{value#Sexp} but channel has \
           layout TODO at %{str_i send_instr}."]
  | Written_mem_value_doesnt_fit_in_cell (write_instr, value) ->
      (* let mem = t.mem_table_info.(mem_idx).src in *)
      (* let mem_cell_layout = Ir_dtype.layout mem.d.dtype in *)
      let value = do_sexp write_instr value in
      (* let value = *)
      (* Ir_dtype.value_of_cint_exn mem.d.dtype value *)
      (* |> Ir_dtype.sexp_of_t_ mem.d.dtype *)
      (* in *)
      Error
        [%string
          "Written value doesnt fit in memory cell: got %{value#Sexp} but \
           memory cell has layout TODO at %{str_i write_instr}."]
  | Unstable_probe (pc, probe) -> (
      match probe with
      | Mid.Probe.Read chan ->
          let chan_decl =
            (Map.find_exn t.chan_of_mid chan).creation_code_pos
          in
          Error
            [%string
              "Unstable waiting %{str_i pc} for send-ready for chan created \
               %{str_l chan_decl}."]
      | Send chan ->
          let chan_decl =
            (Map.find_exn t.chan_of_mid chan).creation_code_pos
          in
          Error
            [%string
              "Unstable waiting %{str_i pc} for send-ready for chan created \
               %{str_l chan_decl}"])
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
    List.partition_map queued_user_ops ~f:(fun { queuer; value; call_site } ->
        match queuer with
        | `Send chan -> First (chan, (value, call_site))
        | `Read chan -> Second (chan, (value, call_site)))
  in
  let to_send =
    Ir.Chan.Map.of_alist_multi to_send
    |> Map.map ~f:(fun l ->
           List.map l ~f:(fun (value, origin) -> { With_origin.value; origin })
           |> Array.of_list)
  in
  let to_read =
    Ir.Chan.Map.of_alist_multi to_read
    |> Map.map ~f:(fun l ->
           List.map l ~f:(fun (value, origin) -> { With_origin.value; origin })
           |> Array.of_list)
  in
  match t.is_done with
  | true -> Error (Error.of_string "Already done.")
  | false ->
      let status =
        let logs, step_err =
          let to_send =
            Map.to_alist to_send
            |> List.map ~f:(fun (chan, to_send) ->
                   ( Map.find_exn t.iports chan,
                     Array.map to_send ~f:With_origin.value ))
          in
          let to_read =
            Map.to_alist to_read
            |> List.map ~f:(fun (chan, to_read) ->
                   ( Map.find_exn t.oports chan,
                     Array.map to_read ~f:With_origin.value ))
          in
          Mid.wait t.i ~max_steps ~to_send ~to_read
        in
        List.iter logs ~f:(fun (tag, log) ->
            let _, _, msg_fn = Hashtbl.find_exn t.info_of_tag tag in
            let msg = msg_fn log in 
            printf "%s" msg);
        resolve_step_err t ~line_numbers ~to_send ~to_read step_err
        |> Result.map_error ~f:Error.of_string
      in
      Result.iter_error status ~f:(fun _ -> t.is_done <- true);
      Result.map status ~f:(fun `Stuck -> ())

let wait' t ?max_steps () =
  print_s [%sexp (wait t ?max_steps () : unit Or_error.t)]

(* module Mem_id_pool = struct type t = { mutable next_id : int; id_of_mem :
   Mid.Mem.t Ir.Mem.Table.t }

   let create () = { next_id = 0; id_of_mem = Ir.Mem.Table.create () }

   let new_id t = t.next_id <- t.next_id + 1; t.next_id - 1

   let get_id t var_id_pool mem = Hashtbl.find_or_add t.id_of_mem mem
   ~default:(fun () -> let mem_idx = new_id t in let helper_reg_var_idx =
   Var_id_pool.new_id var_id_pool (Mem_idx_reg 1) in (helper_reg_var_idx,
   mem_idx)) end *)

let create_t ~seed ir ~user_sendable_ports ~user_readable_ports =
  let info_of_tag = Mid.Tag.Table.create () in
  let new_tag ?(sexper = CInt.sexp_of_t) ?(msg_fn = CInt.to_string) loc =
    let tag = Hashtbl.length info_of_tag in
    Hashtbl.set info_of_tag ~key:tag ~data:(loc, sexper, msg_fn);
    tag
  in

  let mid_of_var = Ir.Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add mid_of_var v ~default:(fun () ->
        let id = Hashtbl.length mid_of_var in
        { Mid.Var.id; bitwidth = v.bitwidth })
  in

  let mid_of_chan = Ir.Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add mid_of_chan c ~default:(fun () ->
        let id = Hashtbl.length mid_of_chan in
        { Mid.Chan.id; bitwidth = c.bitwidth })
  in

  let mid_of_mem = Ir.Mem.Table.create () in
  let of_mem mem =
    Hashtbl.find_or_add mid_of_mem mem ~default:(fun () ->
        let id = Hashtbl.length mid_of_mem in
        { Mid.Mem.id; cell_bitwidth = mem.cell_bitwidth; init = mem.init })
  in

  let of_e e = Ir.Expr.map_vars e ~f:of_v in

  let rec of_stmt stmt =
    match stmt with
    | Ir.Chp.Assign { m; var; expr } ->
        Mid.Stmt.Assign (new_tag ~sexper:m.var_sexper m.cp, of_v var, of_e expr)
    | Nop m -> Nop (new_tag m.cp)
    | Log1 { m; expr; f } -> Log1 (new_tag ~msg_fn:f m.cp, of_e expr)
    | Assert { m; expr; log_e; msg_fn } ->
        Assert (new_tag ~msg_fn m.cp, of_e expr, of_e log_e)
    | Seq { m; ns = stmts } -> Seq (new_tag m.cp, List.map stmts ~f:of_stmt)
    | Par { m; ns = stmts } -> Par (new_tag m.cp, List.map stmts ~f:of_stmt)
    | SelectImm { m; branches; else_ } ->
        let branches =
          List.map branches ~f:(fun (g, stmt) -> (of_e g, of_stmt stmt))
        in
        let else_ =
          match else_ with
          | Some else_ -> of_stmt else_
          | None ->
              let err_msg = "Select branch has no true guards" in
              let err_tag = new_tag ~msg_fn:(fun _ -> err_msg) m.cp in
              let err =
                Mid.Stmt.Assert
                  (err_tag, of_e (Const CInt.zero), of_e (Const CInt.zero))
              in
              err
        in
        SelectImm (new_tag m.cp, branches, else_)
    | Read { m; chan; var } ->
        Read (new_tag ~sexper:m.var_sexper m.cp, of_c chan, of_v var)
    | Send { m; chan; expr } ->
        Send (new_tag ~sexper:m.chan_sexper m.cp, of_c chan, of_e expr)
    | WhileLoop { m; g = expr; n = stmt } ->
        WhileLoop (new_tag m.cp, of_e expr, of_stmt stmt)
    | DoWhile { m; n = stmt; g = expr } ->
        DoWhile (new_tag m.cp, of_stmt stmt, of_e expr)
    | ReadMem { m; mem; idx; var = dst } ->
        ReadMem (new_tag m.cp, of_mem mem, of_e idx, of_v dst)
    | WriteMem { m; mem; idx; expr = value } ->
        WriteMem (new_tag m.cp, of_mem mem, of_e idx, of_e value)
    | Nondeterm_select { m; branches } ->
        let branches =
          List.map branches ~f:(fun (probe, stmt) ->
              let probe =
                match probe with
                | Read chan -> Mid.Probe.Read (of_c chan)
                | Send chan -> Send (of_c chan)
              in
              (probe, of_stmt stmt))
        in
        Nondeterm_select (new_tag m.cp, branches)
  in

  let ir = of_stmt ir in

  (* Now add on all the initializations *)
  let inits =
    Hashtbl.to_alist mid_of_var
    |> Ir.Var.Map.of_alist_exn |> Map.to_alist
    |> List.filter_map ~f:(fun (var, mid_var) ->
           match var.init with
           | Some init -> Some (mid_var, init)
           | None -> None)
    |> List.map ~f:(fun (mid_var, init) ->
           Mid.Stmt.Assign
             (new_tag Code_pos.dummy_loc, mid_var, of_e (Const init)))
  in
  let ir = Mid.Stmt.Seq (new_tag Code_pos.dummy_loc, inits @ [ ir ]) in

  let iports = Map.of_key_set user_sendable_ports ~f:of_c in
  let oports = Map.of_key_set user_readable_ports ~f:of_c in
  let i =
    Mid.create ir ~seed ~iports:(Map.data iports) ~oports:(Map.data oports)
  in

  let var_of_mid =
    Hashtbl.to_alist mid_of_var
    |> List.map ~f:(fun (var, mid) -> (mid, var))
    |> Mid.Var.Map.of_alist_exn
  in

  let chan_of_mid =
    Hashtbl.to_alist mid_of_chan
    |> List.map ~f:(fun (chan, mid) -> (mid, chan))
    |> Mid.Chan.Map.of_alist_exn
  in

  let mem_of_mid =
    Hashtbl.to_alist mid_of_mem
    |> List.map ~f:(fun (mem, mid) -> (mid, mem))
    |> Mid.Mem.Map.of_alist_exn
  in

  {
    i;
    is_done = false;
    info_of_tag;
    var_of_mid;
    chan_of_mid;
    mem_of_mid;
    iports;
    oports;
    queued_user_ops = Queue.create ();
  }

let reset t =
  Mid.reset t.i;
  t.is_done <- false;
  Queue.clear t.queued_user_ops

let simulate ?(seed = 0) (process : Ir.Process.t) =
  let chp =
    let rec extract (proc : Ir.Process.t) =
      match proc.inner with
      | Chp chp -> chp
      | Dflow_iface_on_chp chp -> (* TODO add buffers and stuff? *) chp
      | Subprocs subprocs -> Ir.Chp.par (List.map subprocs ~f:extract)
    in
    extract process
  in
  let user_sendable_ports = process.iports in
  let user_readable_ports = process.oports in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
  let t = create_t ~seed chp ~user_sendable_ports ~user_readable_ports in
  reset t;
  t

let queue_user_io_op t call_site chan value queuer =
  let ivalue = value in
  let chan_bitwidth = chan.Ir.Chan.bitwidth in
  if chan_bitwidth >= CInt.bitwidth ivalue then
    Queue.enqueue t.queued_user_ops
      { Queued_user_op.queuer; value = ivalue; call_site }
  else
    let value = CInt.sexp_of_t value in
    failwith
      [%string
        "Value doesnt fit in chan: got value %{value#Sexp} but channel has \
         bitwidth %{chan_bitwidth#Int}."]

let send t chan value =
  let call_site = Code_pos.psite () in
  match Map.find t.iports chan with
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-sendable chan in \
         Sim.create"
  | Some _ -> queue_user_io_op t call_site chan value (`Send chan)

let read t chan value =
  let call_site = Code_pos.psite () in
  match Map.find t.oports chan with
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-readable chan in \
         Sim.create"
  | Some _ -> queue_user_io_op t call_site chan value (`Read chan)
