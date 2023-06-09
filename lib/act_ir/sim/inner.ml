open! Core
open Utils

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
  module NI = Int

  module N = struct
    module T = struct
      type t =
        | Var of Var_id.t
        | Const of CInt.t
        | Add of NI.t * NI.t
        | Sub_no_underflow of NI.t * NI.t
        | Mul of NI.t * NI.t
        | Div of NI.t * NI.t
        | Mod of NI.t * NI.t
        | LShift of NI.t * NI.t
        | RShift of NI.t * NI.t
        | BitAnd of NI.t * NI.t
        | BitOr of NI.t * NI.t
        | BitXor of NI.t * NI.t
        | Eq0 of NI.t
        | Eq of NI.t * NI.t
        | Ne of NI.t * NI.t
        | Lt of NI.t * NI.t
        | Le of NI.t * NI.t
        | Gt of NI.t * NI.t
        | Ge of NI.t * NI.t
        | Clip of NI.t * int
        | Concat of (NI.t * int) list
        | Log2OneHot of NI.t
        | Return of NI.t
      [@@deriving sexp, hash, equal, compare]
    end

    include Hashable.Make (T)
    include T
  end

  type t = { ns : N.t array } [@@deriving sexp_of]

  let var_ids t =
    Array.to_list t.ns
    |> List.filter_map ~f:(fun v ->
           match v with Var var_id -> Some var_id | _ -> None)

  let dummy_expr = { ns = [| N.Const CInt.zero; N.Return 0 |] }
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
end

module Mem_buff = struct
  type t = {
    (* immutable data *)
    cell_bitwidth : int;
    idx_helper_reg : Var_id.t;
    (* mutable *)
    arr : CInt.t array;
  }
  [@@deriving sexp_of]
end

module Enqueuer_buff = struct
  type t = {
    mutable to_send : CInt.t array;
    var_id : Var_id.t;
    mutable is_done : bool;
    mutable idx : int;
  }
  [@@deriving sexp_of]
end

module Dequeuer_buff = struct
  type t = {
    mutable expected_reads : CInt.t array;
    var_id : Var_id.t;
    mutable idx : int;
  }
  [@@deriving sexp_of]
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
    | Log1 of Expr.t
    | Assert of Expr.t * Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
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
    (* These are ``magic'' instructions that allow user io operations. These
       instruction should be placed immediatly after the assoiated send/read
       instruction *)
    | Send_enqueuer of Enqueuer_idx.t
    | Read_dequeuer of Dequeuer_idx.t
  [@@deriving sexp_of]
end

(* This module doesnt know about dtypes. Everything is just a CInt. This should
   be fairly easy to port into c/c++/rust if we need the extra performance *)
module E = struct
  type t =
    | Uninit_id of Var_id.t * Instr_idx.t
    | Simul_read_write_var of Instr_idx.t * Instr_idx.t * Var_id.t
    | Simul_write_write_var of Instr_idx.t * Instr_idx.t * Var_id.t
    | Select_no_guards_true of Instr_idx.t
    | Select_multiple_guards_true of Instr_idx.t * int list
    | Assert_failure of Instr_idx.t * CInt.t
    | Simul_chan_readers of Instr_idx.t * Instr_idx.t
    | Simul_chan_senders of Instr_idx.t * Instr_idx.t
    | Select_multiple_true_probes of Instr_idx.t * (int * (Probe.t * int)) list
    | Unstable_probe of Instr_idx.t * Probe.t
    | Read_dequeuer_wrong_value of Dequeuer_idx.t * CInt.t * int
    | Mem_out_of_bounds of Instr_idx.t * CInt.t * int
    | User_read_did_not_complete of Dequeuer_idx.t * int
    | User_send_did_not_complete of Enqueuer_idx.t * int
    | Stuck
    | Time_out
  [@@deriving sexp_of]
end

module Var_spec = struct
  type t = { bitwidth : int } [@@deriving sexp_of]
end

module Chan_spec = struct
  type t = { bitwidth : int } [@@deriving sexp_of]
end

module Mem_spec = struct
  type t = {
    cell_bitwidth : int;
    idx_helper_reg : Var_id.t;
    init : CInt.t array;
  }
  [@@deriving sexp_of]
end

module Enqueuer_spec = struct
  type t = { var_id : Var_id.t } [@@deriving sexp_of]
end

module Dequeuer_spec = struct
  type t = { var_id : Var_id.t } [@@deriving sexp_of]
end

module Setup = struct
  type t = {
    assem : N.t array;
    assem_guard_read_ids : Var_id.Set.t array;
    assem_guard_write_ids : Var_id.Set.t array;
    var_specs : Var_spec.t array;
    chan_specs : Chan_spec.t array;
    mem_specs : Mem_spec.t array;
    enqueuer_specs : Enqueuer_spec.t array;
    dequeuer_specs : Dequeuer_spec.t array;
    seed : int;
  }
  [@@deriving sexp_of]
end

module State = struct
  type t = {
    pcs : Instr_idx.t Vec.t;
    var_table : Var_buff.t array;
    chan_table : Chan_buff.t array;
    mem_table : Mem_buff.t array;
    enqueuer_table : Enqueuer_buff.t array;
    dequeuer_table : Dequeuer_buff.t array;
    rng : (Random.State.t[@sexp.opaque]);
  }
  [@@deriving sexp_of]
end

type t = { setup : Setup.t; mutable s : State.t } [@@deriving sexp_of]

let set_enqueuer t ~enqueuer_idx ~is_done ~idx ~to_send ~push_pc =
  let enqueuer = t.s.enqueuer_table.(enqueuer_idx) in
  enqueuer.is_done <- is_done;
  enqueuer.idx <- idx;
  enqueuer.to_send <- to_send;
  Vec.push t.s.pcs push_pc

let set_dequeuer t ~dequeuer_idx ~idx ~expected_reads ~push_pc =
  let dequeuer = t.s.dequeuer_table.(dequeuer_idx) in
  dequeuer.idx <- idx;
  dequeuer.expected_reads <- expected_reads;
  Vec.push t.s.pcs push_pc

let step' t ~pc_idx ~logs =
  let bool_of_cint i =
    match CInt.to_int_exn i with
    | 0 -> false
    | 1 -> true
    | c -> failwith [%string "Simulator bug: unexpected bool value %{c#Int}"]
  in
  let eval_var_table (expr : Expr.t) =
    let reg = Array.create CInt.zero ~len:(Array.length expr.ns) in
    let res = ref None in
    let i = ref 0 in
    let of_bool b = Bool.to_int b |> CInt.of_int in
    while !i < Array.length expr.ns && Option.is_none !res do
      (match expr.ns.(!i) with
      | Var id -> reg.(!i) <- t.s.var_table.(id).value
      | Const c -> reg.(!i) <- c
      | Add (a, b) -> reg.(!i) <- CInt.add reg.(a) reg.(b)
      | Sub_no_underflow (a, b) -> reg.(!i) <- CInt.sub reg.(a) reg.(b)
      | Mul (a, b) -> reg.(!i) <- CInt.mul reg.(a) reg.(b)
      | Div (a, b) -> reg.(!i) <- CInt.div reg.(a) reg.(b)
      | Mod (a, b) -> reg.(!i) <- CInt.mod_ reg.(a) reg.(b)
      | LShift (a, b) -> reg.(!i) <- CInt.left_shift reg.(a) ~amt:reg.(b)
      | RShift (a, b) -> reg.(!i) <- CInt.right_shift reg.(a) ~amt:reg.(b)
      | BitAnd (a, b) -> reg.(!i) <- CInt.bit_and reg.(a) reg.(b)
      | BitOr (a, b) -> reg.(!i) <- CInt.bit_or reg.(a) reg.(b)
      | BitXor (a, b) -> reg.(!i) <- CInt.bit_xor reg.(a) reg.(b)
      | Eq0 a -> reg.(!i) <- CInt.eq reg.(a) CInt.zero |> of_bool
      | Eq (a, b) -> reg.(!i) <- CInt.eq reg.(a) reg.(b) |> of_bool
      | Ne (a, b) -> reg.(!i) <- CInt.ne reg.(a) reg.(b) |> of_bool
      | Lt (a, b) -> reg.(!i) <- CInt.lt reg.(a) reg.(b) |> of_bool
      | Le (a, b) -> reg.(!i) <- CInt.le reg.(a) reg.(b) |> of_bool
      | Gt (a, b) -> reg.(!i) <- CInt.gt reg.(a) reg.(b) |> of_bool
      | Ge (a, b) -> reg.(!i) <- CInt.ge reg.(a) reg.(b) |> of_bool
      | Clip (a, bits) -> reg.(!i) <- CInt.clip reg.(a) ~bits
      | Concat l ->
          let _, v =
            List.fold ~init:(0, CInt.zero) l ~f:(fun (offset, v) (x, bits) ->
                ( offset + bits,
                  CInt.clip reg.(x) ~bits
                  |> CInt.left_shift' ~amt:offset
                  |> CInt.add v ))
          in
          reg.(!i) <- v
      | Log2OneHot a ->
          let b = CInt.bitwidth reg.(a) in
          assert (CInt.(eq zero (sub reg.(a) (left_shift' one ~amt:(b - 1)))));
          reg.(!i) <- CInt.of_int (b - 1)
      | Return a -> res := Some reg.(a));
      incr i
    done;
    Option.value_exn !res
  in
  let eval_bool expr = eval_var_table expr |> bool_of_cint in
  let set_var_table ~var_id ~value =
    t.s.var_table.(var_id).value <- value;
    t.s.var_table.(var_id).is_inited <- true
  in
  let at_var_table ~var_id =
    assert t.s.var_table.(var_id).is_inited;
    t.s.var_table.(var_id).value
  in

  let find_rw t var_id ~ignore ~get_ids =
    let is_rw pc =
      (not (Var_id.equal ignore pc)) && Set.mem (get_ids pc) var_id
    in
    let rw_id_of_chan (chan : Chan_buff.t) =
      let send_i, read_i = (chan.send_instr, chan.read_instr) in
      if chan.send_ready && is_rw send_i then Some send_i
      else if chan.read_ready && is_rw read_i then Some read_i
      else None
    in
    Array.find_map t.s.chan_table ~f:rw_id_of_chan
    |> some_or_thunk ~f:(fun () -> Vec.find t.s.pcs ~f:is_rw)
    |> Option.value_exn
  in
  let find_reader var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:(fun i ->
        t.setup.assem_guard_read_ids.(i))
  in
  let find_writer var_id ~ignore =
    find_rw t var_id ~ignore ~get_ids:(fun i ->
        t.setup.assem_guard_write_ids.(i))
  in
  let guard pc =
    let read_ids = t.setup.assem_guard_read_ids.(pc) in
    let write_ids = t.setup.assem_guard_write_ids.(pc) in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id -> t.s.var_table.(read_id).write_ct > 0)
      |> to_unit_result ~f:(fun var_id ->
             let write_pc = find_writer var_id ~ignore:pc in
             E.Simul_read_write_var (pc, write_pc, var_id))
    in
    let%bind.Result () =
      Set.find read_ids ~f:(fun read_id ->
          not t.s.var_table.(read_id).is_inited)
      |> to_unit_result ~f:(fun read_id -> E.Uninit_id (read_id, pc))
    in
    let%bind.Result () =
      Set.find write_ids ~f:(fun write_id ->
          t.s.var_table.(write_id).read_ct > 0)
      |> to_unit_result ~f:(fun var_id ->
             let read_pc = find_reader var_id ~ignore:pc in
             E.Simul_read_write_var (read_pc, pc, var_id))
    in
    let%map.Result () =
      Set.find write_ids ~f:(fun write_id ->
          t.s.var_table.(write_id).write_ct > 0)
      |> to_unit_result ~f:(fun var_id ->
             let write_pc_2 = find_writer var_id ~ignore:pc in
             E.Simul_write_write_var (pc, write_pc_2, var_id))
    in
    Set.iter read_ids ~f:(fun read_id ->
        t.s.var_table.(read_id).read_ct <- t.s.var_table.(read_id).read_ct + 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.s.var_table.(write_id).write_ct <-
          t.s.var_table.(write_id).write_ct + 1)
  in
  let set_pc_and_guard ~pc_idx new_pc =
    Vec.set t.s.pcs pc_idx new_pc |> fun () -> guard new_pc
  in
  let push_pc_and_guard new_pc =
    Vec.push t.s.pcs new_pc |> fun () -> guard new_pc
  in
  let unguard pc =
    let read_ids = t.setup.assem_guard_read_ids.(pc) in
    let write_ids = t.setup.assem_guard_write_ids.(pc) in
    Set.iter read_ids ~f:(fun read_id ->
        t.s.var_table.(read_id).read_ct <- t.s.var_table.(read_id).read_ct - 1);
    Set.iter write_ids ~f:(fun write_id ->
        t.s.var_table.(write_id).write_ct <-
          t.s.var_table.(write_id).write_ct - 1)
  in

  let step_chan (chan : Chan_buff.t) =
    if chan.read_ready && chan.send_ready then (
      chan.read_ready <- false;
      chan.send_ready <- false;
      unguard chan.read_instr;
      unguard chan.send_instr;
      let value = eval_var_table chan.send_expr in
      (* check_value_fits_width chan.bitwidth ~value *)
      (* let var_width = t.s.var_table.(chan.read_dst_var_id).bitwidth in *)
      (* check_value_fits_width var_width ~value *)
      let () = set_var_table ~var_id:chan.read_dst_var_id ~value in
      let () =
        Vec.extend t.s.pcs [ chan.read_instr + 1; chan.send_instr + 1 ]
      in
      let%bind.Result () = guard (chan.read_instr + 1) in
      guard (chan.send_instr + 1))
    else Ok ()
  in

  let step_select l ~else_ ~pc ~pc_idx =
    let branches =
      List.mapi l ~f:(fun idx (expr, instr) ->
          let guard = eval_bool expr in
          (guard, instr, idx))
    in
    let true_branches =
      List.filter branches ~f:(fun (g, _, _) -> g)
      |> List.map ~f:(fun (_, instr, idx) -> (instr, idx))
    in
    match (true_branches, else_) with
    | [], None -> Error (E.Select_no_guards_true pc)
    | [], Some else_ -> set_pc_and_guard ~pc_idx else_
    | [ (instr, _) ], _ -> set_pc_and_guard ~pc_idx instr
    | l, _ -> Error (E.Select_multiple_guards_true (pc, List.map l ~f:snd))
  in

  let pc = Vec.at t.s.pcs pc_idx in
  match t.setup.assem.(pc) with
  | End ->
      (* unguard pc; *)
      Vec.remove t.s.pcs pc_idx;
      Ok ()
  | Nop ->
      (* unguard pc; *)
      set_pc_and_guard ~pc_idx (pc + 1)
  | Assign (var_id, expr) ->
      unguard pc;
      let value = eval_var_table expr in

      (* let var_width = t.s.var_table.(var_id).bitwidth in *)
      (* check_value_fits_width var_width ~value *)
      let () = set_var_table ~var_id ~value in
      set_pc_and_guard ~pc_idx (pc + 1)
  | Assert (expr, log_e) -> (
      unguard pc;
      let expr = eval_bool expr in
      match expr with
      | true -> set_pc_and_guard ~pc_idx (pc + 1)
      | false ->
          let log_e = eval_var_table log_e in
          Error (E.Assert_failure (pc, log_e)))
  | Log1 expr ->
      unguard pc;
      let expr = eval_var_table expr in
      Vec.push logs (pc, expr);
      (* printf "TODO: %s" [%string "%{expr#CInt}"]; *)
      set_pc_and_guard ~pc_idx (pc + 1)
  | Par instrs ->
      (* unguard pc; *)
      Vec.remove t.s.pcs pc_idx;
      Result.all_unit (List.map instrs ~f:push_pc_and_guard)
  | ParJoin d -> (
      (* unguard pc; *)
      d.curr_ct <- d.curr_ct + 1;
      match Int.equal d.max_ct d.curr_ct with
      | true ->
          d.curr_ct <- 0;
          set_pc_and_guard ~pc_idx (pc + 1)
      | false ->
          Vec.remove t.s.pcs pc_idx;
          Ok ())
  | Jump inst ->
      (* unguard pc; *)
      set_pc_and_guard ~pc_idx inst
  | JumpIfFalse (expr, inst) ->
      unguard pc;
      let expr = eval_bool expr in
      set_pc_and_guard ~pc_idx (if expr then pc + 1 else inst)
  | SelectImmElse (l, else_) ->
      unguard pc;
      step_select l ~else_:(Some else_) ~pc ~pc_idx
  | Read (dst_id, chan_idx) ->
      (* unguard pc; *)
      let chan = t.s.chan_table.(chan_idx) in
      if chan.read_ready then Error (E.Simul_chan_readers (chan.read_instr, pc))
      else (
        chan.read_ready <- true;
        chan.read_instr <- pc;
        chan.read_dst_var_id <- dst_id;
        Vec.remove t.s.pcs pc_idx;
        Vec.iter chan.select_probe_read_ready
          ~f:(fun (waiting_pc, other_probes) ->
            (* First go and turn off all the other probes! *)
            List.iter other_probes ~f:(fun (oprobe, oinstr) ->
                match oprobe with
                | Read_ready ochan_idx ->
                    assert (not (Int.equal ochan_idx chan_idx));
                    Vec.filter
                      t.s.chan_table.(ochan_idx).select_probe_read_ready
                      ~f:(fun (pc, _) -> not (Int.equal pc oinstr))
                | Send_ready ochan_idx ->
                    assert (not (Int.equal ochan_idx chan_idx));
                    Vec.filter
                      t.s.chan_table.(ochan_idx).select_probe_send_ready
                      ~f:(fun (pc, _) -> not (Int.equal pc oinstr)));
            (* It doesnt need gaurding becaus the waiting_pc is required to be a
               WaitUntilReadable or WaitUntilSendable node, which has no
               read/written variables *)
            Vec.push t.s.pcs waiting_pc);
        step_chan chan)
  | Send (expr, chan_idx) ->
      (* unguard pc; *)
      let chan = t.s.chan_table.(chan_idx) in
      if chan.send_ready then Error (E.Simul_chan_senders (chan.send_instr, pc))
      else (
        chan.send_ready <- true;
        chan.send_instr <- pc;
        chan.send_expr <- expr;
        Vec.remove t.s.pcs pc_idx;
        Vec.iter chan.select_probe_send_ready
          ~f:(fun (waiting_pc, other_probes) ->
            (* First go and turn off all the other probes! *)
            List.iter other_probes ~f:(fun (oprobe, oinstr) ->
                match oprobe with
                | Read_ready ochan_idx ->
                    assert (not (Int.equal ochan_idx chan_idx));
                    Vec.filter
                      t.s.chan_table.(ochan_idx).select_probe_read_ready
                      ~f:(fun (pc, _) -> not (Int.equal pc oinstr))
                | Send_ready ochan_idx ->
                    assert (not (Int.equal ochan_idx chan_idx));
                    Vec.filter
                      t.s.chan_table.(ochan_idx).select_probe_send_ready
                      ~f:(fun (pc, _) -> not (Int.equal pc oinstr)));
            (* It doesnt need gaurding becaus the waiting_pc is required to be a
               WaitUntilReadable or WaitUntilSendable node, which has no
               read/written variables *)
            Vec.push t.s.pcs waiting_pc);
        step_chan chan)
  | SelectProbes probe_select -> (
      (* TODO *)
      (* unguard pc; *)
      (* first check that how many probes are already true. If it is more than
         one, this is an error *)
      match
        List.mapi probe_select ~f:(fun i probe -> (i, probe))
        |> List.filter ~f:(fun (_, (probe, _)) ->
               match probe with
               | Read_ready chan_idx -> t.s.chan_table.(chan_idx).read_ready
               | Send_ready chan_idx -> t.s.chan_table.(chan_idx).send_ready)
      with
      | [] ->
          Vec.remove t.s.pcs pc_idx;
          List.iter probe_select ~f:(fun (probe, instr) ->
              let other_instrs =
                List.filter probe_select ~f:(fun (_, i) ->
                    not (Int.equal i instr))
              in
              match probe with
              | Read_ready chan_idx ->
                  Vec.push t.s.chan_table.(chan_idx).select_probe_read_ready
                    (instr, other_instrs)
              | Send_ready chan_idx ->
                  Vec.push t.s.chan_table.(chan_idx).select_probe_send_ready
                    (instr, other_instrs));
          Ok ()
      | [ (_, (_, instr)) ] -> set_pc_and_guard ~pc_idx instr
      | multiple_probes ->
          Error (E.Select_multiple_true_probes (pc, multiple_probes)))
  | SelectProbes_AssertStable (tprobe, fprobes) ->
      (* unguard pc; *)
      let errors =
        (let is_ready =
           match tprobe with
           | Read_ready chan_idx -> t.s.chan_table.(chan_idx).read_ready
           | Send_ready chan_idx -> t.s.chan_table.(chan_idx).send_ready
         in
         if is_ready then Ok () else Error (E.Unstable_probe (pc, tprobe)))
        :: List.map fprobes ~f:(fun probe ->
               let is_ready =
                 match probe with
                 | Read_ready chan_idx -> t.s.chan_table.(chan_idx).read_ready
                 | Send_ready chan_idx -> t.s.chan_table.(chan_idx).send_ready
               in
               if is_ready then Error (E.Unstable_probe (pc, probe)) else Ok ())
      in
      let%bind.Result () = Result.all_unit errors in
      set_pc_and_guard ~pc_idx (pc + 1)
  | Send_enqueuer enq_idx ->
      (* unguard pc; *)
      let enqueuer = t.s.enqueuer_table.(enq_idx) in
      assert (not enqueuer.is_done);
      if enqueuer.idx >= Array.length enqueuer.to_send then (
        Vec.remove t.s.pcs pc_idx;
        enqueuer.is_done <- true;
        Ok ())
      else
        let value = enqueuer.to_send.(enqueuer.idx) in
        enqueuer.idx <- enqueuer.idx + 1;
        set_var_table ~var_id:enqueuer.var_id ~value;
        set_pc_and_guard ~pc_idx (pc - 1)
  | Read_dequeuer deq_idx ->
      let dequeuer = t.s.dequeuer_table.(deq_idx) in
      unguard pc;
      let value = at_var_table ~var_id:dequeuer.var_id in
      let expected = dequeuer.expected_reads.(dequeuer.idx) in
      if not (CInt.equal value expected) then
        Error (E.Read_dequeuer_wrong_value (deq_idx, value, dequeuer.idx))
      else (
        dequeuer.idx <- dequeuer.idx + 1;
        if dequeuer.idx >= Array.length dequeuer.expected_reads then (
          Vec.remove t.s.pcs pc_idx;
          Ok ())
        else set_pc_and_guard ~pc_idx (pc - 1))
  | ReadMem (idx_expr, dst_id, _, mem_idx) ->
      unguard pc;
      let mem = t.s.mem_table.(mem_idx) in
      let idx = eval_var_table idx_expr in
      if CInt.(idx < zero) || CInt.(idx >= (Array.length mem.arr |> of_int))
      then Error (E.Mem_out_of_bounds (pc, idx, Array.length mem.arr))
      else
        let value = mem.arr.(CInt.to_int_exn idx) in
        (* let var_width = t.s.var_table.(dst_id).bitwidth in *)
        (* check_value_fits_width var_width ~value *)
        let () = set_var_table ~var_id:dst_id ~value in
        set_pc_and_guard ~pc_idx (pc + 1)
  | WriteMem (idx_expr, src_expr, _, mem_idx) ->
      unguard pc;
      let mem = t.s.mem_table.(mem_idx) in
      let idx = eval_var_table idx_expr in
      if CInt.(idx < zero) || CInt.(idx >= (Array.length mem.arr |> of_int))
      then Error (E.Mem_out_of_bounds (pc, idx, Array.length mem.arr))
      else
        let value = eval_var_table src_expr in
        (* check_value_fits_width mem.cell_bitwidth ~value *)
        let () = mem.arr.(CInt.to_int_exn idx) <- value in
        set_pc_and_guard ~pc_idx (pc + 1)

let wait t ~max_steps () =
  let logs = Vec.create ~cap:10 ~default:(0, CInt.zero) in
  let step _ =
    if Vec.is_empty t.s.pcs then
      (* check whether we need error because the queuers are unfinished *)
      match
        Array.findi t.s.dequeuer_table ~f:(fun _ deq ->
            deq.idx < Array.length deq.expected_reads)
      with
      | Some (deq_idx, deq) ->
          let read_idx = deq.idx in
          `Return (E.User_read_did_not_complete (deq_idx, read_idx))
      | None -> (
          match
            Array.findi t.s.enqueuer_table ~f:(fun _ enq -> not enq.is_done)
          with
          | Some (enq_idx, enq) ->
              let send_idx = enq.idx in
              `Return (E.User_send_did_not_complete (enq_idx, send_idx))
          | None -> `Return E.Stuck)
    else
      let status =
        step' t
          ~pc_idx:(Random.State.int_incl t.s.rng 0 (Vec.length t.s.pcs - 1))
          ~logs
      in
      match status with Ok () -> `Continue | Error e -> `Return e
  in

  let status = for_loop_else max_steps ~f:step ~else_:E.Time_out in
  (logs, status)

let create_state (setup : Setup.t) =
  let var_table =
    Array.map setup.var_specs ~f:(fun spec ->
        let bitwidth = spec.bitwidth in
        {
          Var_buff.bitwidth;
          value = CInt.zero;
          is_inited = false;
          read_ct = 0;
          write_ct = 0;
        })
  in
  let chan_table =
    Array.map setup.chan_specs ~f:(fun spec ->
        {
          Chan_buff.bitwidth = spec.bitwidth;
          read_ready = false;
          send_ready = false;
          read_instr = Instr_idx.dummy_val;
          send_instr = Instr_idx.dummy_val;
          read_dst_var_id = 0;
          send_expr = Expr.dummy_expr;
          select_probe_read_ready = Vec.create ~cap:10 ~default:(0, []);
          select_probe_send_ready = Vec.create ~cap:10 ~default:(0, []);
        })
  in
  let mem_table =
    Array.map setup.mem_specs ~f:(fun spec ->
        let arr = Array.copy spec.init in
        let cell_bitwidth = spec.cell_bitwidth in
        let idx_helper_reg = spec.idx_helper_reg in
        { Mem_buff.cell_bitwidth; arr; idx_helper_reg })
  in
  let enqueuer_table =
    Array.map setup.enqueuer_specs ~f:(fun spec ->
        {
          Enqueuer_buff.to_send = [||];
          var_id = spec.var_id;
          is_done = true;
          idx = 0;
        })
  in
  let dequeuer_table =
    Array.map setup.dequeuer_specs ~f:(fun spec ->
        { Dequeuer_buff.expected_reads = [||]; var_id = spec.var_id; idx = 0 })
  in
  let pcs = Vec.of_array [| 0 |] ~default:(-1) in
  let rng = Random.State.make [| setup.seed |] in
  {
    State.var_table;
    chan_table;
    mem_table;
    enqueuer_table;
    dequeuer_table;
    pcs;
    rng;
  }

let create ~assem ~assem_guard_read_ids ~assem_guard_write_ids ~vars:var_specs
    ~chans:chan_specs ~mems:mem_specs ~enqueuers:enqueuer_specs
    ~dequeuers:dequeuer_specs ~seed =
  let setup =
    {
      Setup.assem;
      assem_guard_read_ids;
      assem_guard_write_ids;
      var_specs;
      chan_specs;
      mem_specs;
      enqueuer_specs;
      dequeuer_specs;
      seed;
    }
  in
  { setup; s = create_state setup }

let reset t = t.s <- create_state t.setup
