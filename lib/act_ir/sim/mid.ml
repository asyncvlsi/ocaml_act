open! Core
open Utils

module Tag = struct
  include Int

  let dummy = 0
end

module Expr = Ir.Expr

module Var = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Chan = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Mem = struct
  module T = struct
    type t = {
      id : int;
      cell_bitwidth : int;
      init : (CInt.t array[@hash.ignore] [@sexp.opaque]);
    }
    [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Probe = struct
  type t = Read of Chan.t | Send of Chan.t [@@deriving sexp_of]
end

module Stmt = struct
  type t =
    | Nop of Tag.t
    | Assign of Tag.t * Var.t * Var.t Expr.t
    | Log1 of Tag.t * Var.t Expr.t
    | Assert of Tag.t * Var.t Expr.t * Var.t Expr.t
    | Seq of Tag.t * t list
    | Par of Tag.t * t list
    | SelectImm of Tag.t * (Var.t Expr.t * t) list * t
    | Read of Tag.t * Chan.t * Var.t
    | Send of Tag.t * Chan.t * Var.t Expr.t
    | WhileLoop of Tag.t * Var.t Expr.t * t
    | DoWhile of Tag.t * t * Var.t Expr.t
    | ReadMem of Tag.t * Mem.t * Var.t Expr.t * Var.t
    | WriteMem of Tag.t * Mem.t * Var.t Expr.t * Var.t Expr.t
    | Nondeterm_select of Tag.t * (Probe.t * t) list
  [@@deriving sexp_of]
end

(* a few simle optimizations on the Stmt datatype to generate better byte
   code *)
let rec flatten stmt =
  let flatten_brs branches =
    List.map branches ~f:(fun (g, stmt) -> (g, flatten stmt))
  in
  match stmt with
  | Stmt.Nop tag -> Stmt.Nop tag
  | Assign (tag, var, expr) -> Assign (tag, var, expr)
  | Log1 (tag, expr) -> Log1 (tag, expr)
  | Assert (tag, e1, e2) -> Assert (tag, e1, e2)
  | Seq (tag, ns) -> (
      let ns =
        List.map ns ~f:flatten
        |> List.filter ~f:(fun n -> match n with Nop _ -> false | _ -> true)
        |> List.concat_map ~f:(fun n ->
               match n with Seq (_, ns) -> ns | _ -> [ n ])
      in
      match ns with [] -> Nop tag | [ n ] -> n | ls -> Seq (tag, ls))
  | Par (tag, ns) -> (
      let ns =
        List.map ns ~f:flatten
        |> List.filter ~f:(fun n -> match n with Nop _ -> false | _ -> true)
        |> List.concat_map ~f:(fun n ->
               match n with Par (_, ns) -> ns | _ -> [ n ])
      in
      match ns with [] -> Nop tag | [ n ] -> n | ls -> Par (tag, ls))
  | Read (tag, chan, var) -> Read (tag, chan, var)
  | Send (tag, chan, var) -> Send (tag, chan, var)
  | ReadMem (tag, mem, expr, dst) -> ReadMem (tag, mem, expr, dst)
  | WriteMem (tag, mem, expr, value) -> WriteMem (tag, mem, expr, value)
  | SelectImm (tag, branches, else_) ->
      SelectImm (tag, flatten_brs branches, flatten else_)
  | WhileLoop (tag, g, stmt) -> WhileLoop (tag, g, flatten stmt)
  | DoWhile (tag, stmt, g) -> DoWhile (tag, flatten stmt, g)
  | Nondeterm_select (tag, branches) ->
      Nondeterm_select (tag, flatten_brs branches)

module Var_id_src = struct
  type t =
    | Var of Var.t
    | Mem_idx_reg of (int * Mem.t)
    | Read_deq_reg of int
    | Send_enq_reg of int
  [@@deriving sexp_of]

  let bitwidth t =
    match t with
    | Var var -> var.bitwidth
    | Mem_idx_reg (bitwidth, _) | Read_deq_reg bitwidth | Send_enq_reg bitwidth
      ->
        bitwidth
end

module Var_buff_info = struct
  type t = { src : Var_id_src.t } [@@deriving sexp_of]
end

module Chan_buff_info = struct
  type t = { src : Chan.t } [@@deriving sexp_of]
end

module Mem_buff_info = struct
  type t = { src : Mem.t } [@@deriving sexp_of]
end

module Enqueuer_info = struct
  type t = { inner_chan : Inner.Chan_id.t; chan : Chan.t } [@@deriving sexp_of]
end

module Dequeuer_info = struct
  type t = {
    inner_chan : Inner.Chan_id.t;
    chan : Chan.t;
    sexper : CInt.t -> Sexp.t;
  }
  [@@deriving sexp_of]
end

module Queued_user_op = struct
  type t = {
    queuer : [ `Send of Inner.Enqueuer_idx.t | `Read of Inner.Dequeuer_idx.t ];
    chan_instr : Inner.Instr_idx.t;
    value : CInt.t;
    call_site : Code_pos.t;
  }
  [@@deriving sexp_of]
end

type t = {
  i : Inner.t;
  mutable is_done : bool;
  (* error message helpers *)
  tag_of_assem_idx : Tag.t array;
  var_table_info : Var_buff_info.t array;
  chan_table_info : Chan_buff_info.t array;
  mem_table_info : Mem_buff_info.t array;
  enqueuer_table_info : Enqueuer_info.t array;
  dequeuer_table_info : Dequeuer_info.t array;
  (* io helpers *)
  all_enqueuers : (Inner.Instr_idx.t * Inner.Enqueuer_idx.t) Chan.Map.t;
  all_dequeuers : (Inner.Instr_idx.t * Inner.Dequeuer_idx.t) Chan.Map.t;
}
[@@deriving sexp_of]

module E = struct
  type t =
    | Uninit_id of Var.t * Tag.t
    | Simul_read_write_var of Tag.t * Tag.t * Var.t
    | Simul_write_write_var of Tag.t * Tag.t * Var.t
    | Simul_mem_access of Tag.t * Tag.t * Mem.t
    | Select_no_guards_true of Tag.t
    | Select_multiple_guards_true of Tag.t * int list
    | Assert_failure of Tag.t * CInt.t
    | Simul_chan_readers of Tag.t * Tag.t
    | Simul_chan_senders of Tag.t * Tag.t
    | Select_multiple_true_probes of Tag.t * (int * (Probe.t * int)) list
    | Unstable_probe of Tag.t * Probe.t
    | Read_dequeuer_wrong_value of Chan.t * CInt.t * int
    | Mem_out_of_bounds of Tag.t * CInt.t * int
    | User_read_did_not_complete of Chan.t * int
    | User_send_did_not_complete of Chan.t * int
    | Stuck
    | Time_out
  [@@deriving sexp_of]
end

let tag_of_pc t pc = t.tag_of_assem_idx.(pc)

let map_step_err t e =
  let of_c c = t.chan_table_info.(c).src in
  let of_v v =
    match t.var_table_info.(v).src with
    | Var var -> var
    | _ -> failwith "unreachable (or bug)"
  in
  let of_pc pc = tag_of_pc t pc in
  let of_probe probe =
    match probe with
    | Inner.Probe.Read_ready c -> Probe.Read (of_c c)
    | Send_ready c -> Send (of_c c)
  in
  match e with
  | Inner.E.Stuck -> E.Stuck
  | Time_out -> Time_out
  | User_read_did_not_complete (deq_idx, read_idx) ->
      let chan = t.dequeuer_table_info.(deq_idx).chan in
      User_read_did_not_complete (chan, read_idx)
  | User_send_did_not_complete (enq_idx, send_idx) ->
      let chan = t.enqueuer_table_info.(enq_idx).chan in
      User_send_did_not_complete (chan, send_idx)
  | Uninit_id (var, pc) -> Uninit_id (of_v var, of_pc pc)
  | Simul_chan_senders (pc1, pc2) -> Simul_chan_senders (of_pc pc1, of_pc pc2)
  | Simul_chan_readers (pc1, pc2) -> Simul_chan_readers (of_pc pc1, of_pc pc2)
  | Assert_failure (pc, cint) -> Assert_failure (of_pc pc, cint)
  | Simul_read_write_var (read_pc, write_pc, var) ->
      Simul_read_write_var (of_pc read_pc, of_pc write_pc, of_v var)
  | Simul_write_write_var (pc1, pc2, var) -> (
      match t.var_table_info.(var).src with
      | Var var -> Simul_write_write_var (of_pc pc1, of_pc pc2, var)
      | Mem_idx_reg (_, mem) -> Simul_mem_access (of_pc pc1, of_pc pc2, mem)
      | Read_deq_reg _ | Send_enq_reg _ -> failwith "unreachable")
  | Select_no_guards_true pc -> Select_no_guards_true (of_pc pc)
  | Select_multiple_guards_true (pc, branch_idxs) ->
      Select_multiple_guards_true (of_pc pc, branch_idxs)
  | Read_dequeuer_wrong_value (deq_idx, actual, expected_idx) ->
      let chan = t.dequeuer_table_info.(deq_idx).chan in
      Read_dequeuer_wrong_value (chan, actual, expected_idx)
  | Mem_out_of_bounds (pc, idx, len) -> Mem_out_of_bounds (of_pc pc, idx, len)
  | Unstable_probe (pc, probe) -> Unstable_probe (of_pc pc, of_probe probe)
  | Select_multiple_true_probes (pc, true_probes) ->
      Select_multiple_true_probes
        ( of_pc pc,
          List.map true_probes ~f:(fun (i1, (probe, i2)) ->
              (i1, (of_probe probe, i2))) )

let wait t ~max_steps ~to_send ~to_read =
  List.iter to_send ~f:(fun (chan, to_send) ->
      let send_instr, enqueuer_idx = Map.find_exn t.all_enqueuers chan in
      Inner.set_enqueuer t.i ~enqueuer_idx ~is_done:false ~idx:0 ~to_send
        ~push_pc:(send_instr + 1));
  List.iter to_read ~f:(fun (chan, expected_reads) ->
      let read_instr, dequeuer_idx = Map.find_exn t.all_dequeuers chan in
      Inner.set_dequeuer t.i ~dequeuer_idx ~idx:0 ~expected_reads
        ~push_pc:read_instr);
  let logs, step_err = Inner.wait t.i ~max_steps () in
  let logs =
    Vec.to_array logs |> Array.to_list
    |> List.map ~f:(fun (pc, log) -> (tag_of_pc t pc, log))
  in
  let step_err = map_step_err t step_err in
  (logs, step_err)

module Assem_builder = struct
  type t = { assem : (Inner.N.t * Tag.t) Vec.t }

  let create () =
    { assem = Vec.create ~cap:10 ~default:(Inner.N.End, Tag.dummy) }

  let push t ~tag (instr : Inner.N.t) =
    Vec.push t.assem (instr, tag);
    Vec.length t.assem - 1

  let edit t ~tag idx (instr : Inner.N.t) = Vec.set t.assem idx (instr, tag)
  let next_idx t = Vec.length t.assem
  let assem_array t = Vec.to_array t.assem
end

module Var_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_var : Inner.Var_id.t Var.Table.t;
    src_of_id : Var_id_src.t Inner.Var_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      id_of_var = Var.Table.create ();
      src_of_id = Inner.Var_id.Table.create ();
    }

  let new_id t src =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.set t.src_of_id ~key:id ~data:src;
    id

  let to_assem_id t var =
    Hashtbl.find_or_add t.id_of_var var ~default:(fun () ->
        new_id t (Var_id_src.Var var))
end

module Chan_id_pool = struct
  type t = { mutable next_id : int; id_of_chan : Inner.Chan_id.t Chan.Table.t }

  let create () = { next_id = 0; id_of_chan = Chan.Table.create () }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t chan =
    Hashtbl.find_or_add t.id_of_chan chan ~default:(fun () -> new_id t)
end

module Mem_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_mem : (Inner.Var_id.t * Inner.Mem_id.t) Mem.Table.t;
  }

  let create () = { next_id = 0; id_of_mem = Mem.Table.create () }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t var_id_pool mem =
    Hashtbl.find_or_add t.id_of_mem mem ~default:(fun () ->
        let mem_idx = new_id t in
        let helper_reg_var_idx =
          Var_id_pool.new_id var_id_pool (Mem_idx_reg (1, mem))
        in
        (helper_reg_var_idx, mem_idx))
end

let inner_n_get_read_ids t =
  (match t with
  | Inner.N.End | Nop | Par _ | ParJoin _ -> []
  | Read (_, _) | Jump _ -> []
  | Log1 expr -> Inner.Expr.var_ids expr
  | Assign (_, expr) -> Inner.Expr.var_ids expr
  | Assert (expr, log_e) -> Inner.Expr.var_ids expr @ Inner.Expr.var_ids log_e
  | JumpIfFalse (expr, _) -> Inner.Expr.var_ids expr
  | SelectImm l | SelectImmElse (l, _) ->
      List.concat_map l ~f:(fun (expr, _) -> Inner.Expr.var_ids expr)
  | Send (expr, _) -> Inner.Expr.var_ids expr
  | Send_enqueuer _ -> []
  | Read_dequeuer _ ->
      (* intentionally none, since this shouldnt be threaded *) []
  | ReadMem (idx_expr, _, _, _) -> Inner.Expr.var_ids idx_expr
  | WriteMem (idx_expr, src_expr, _, _) ->
      Inner.Expr.var_ids idx_expr @ Inner.Expr.var_ids src_expr
  | SelectProbes _ | SelectProbes_AssertStable _ -> [])
  |> Inner.Var_id.Set.of_list

let inner_n_get_write_ids t =
  (match t with
  | Inner.N.End | Nop | Par _ | ParJoin _ -> []
  | Log1 _ | Assert _ | JumpIfFalse (_, _) | Jump _ -> []
  | SelectImm _ | SelectImmElse _ | Send (_, _) -> []
  | Assign (id, _) -> [ id ]
  | Read (var_id, _) -> [ var_id ]
  | Send_enqueuer _ ->
      (* intentionally none, since this shouldnt be threaded *) []
  | Read_dequeuer _ -> []
  | ReadMem (_, dst, mem_idx_reg, _) -> [ dst; mem_idx_reg ]
  | WriteMem (_, _, mem_idx_reg, _) -> [ mem_idx_reg ]
  | SelectProbes _ | SelectProbes_AssertStable _ -> [])
  |> Inner.Var_id.Set.of_list

let create_t ~seed (ir : Stmt.t) ~user_sendable_ports ~user_readable_ports =
  let ab = Assem_builder.create () in
  let push_instr ~tag instr = Assem_builder.push ab ~tag instr in
  let edit_instr ~tag idx instr = Assem_builder.edit ab ~tag idx instr in

  let var_id_pool = Var_id_pool.create () in
  let convert_id id = Var_id_pool.to_assem_id var_id_pool id in
  (* Turns an Ir_expr into a Inner.Expr. Inner.Expr is a flat array. This code
     dedupicates repeated nodes. *)
  let convert_expr' expr =
    let ns = Vec.create ~cap:10 ~default:(Inner.Expr.N.Const CInt.zero) in
    let ni_of_n = Inner.Expr.N.Table.create () in
    let push n =
      Hashtbl.find_or_add ni_of_n n ~default:(fun () ->
          Vec.push ns n;
          Vec.length ns - 1)
    in
    let push' n = ignore (push n : Inner.Expr.NI.t) in
    let rec convert x =
      match x with
      | Ir.Expr.Var var_id -> push (Var (convert_id var_id))
      | Const c -> push (Const c)
      | Add (a, b) -> push (Add (convert a, convert b))
      | Sub_no_wrap (a, b) ->
          let a, b = (convert a, convert b) in
          push (Sub_no_underflow (a, b))
      | Sub_wrap (a, b, bits) ->
          let a, b = (convert a, convert b) in
          let p2bits = push (Const CInt.(left_shift one ~amt:(of_int bits))) in
          let a = push (Clip (a, bits)) in
          let a = push (BitOr (a, p2bits)) in
          let b = push (Clip (b, bits)) in
          let diff = push (Sub_no_underflow (a, b)) in
          push (Clip (diff, bits))
      | Mul (a, b) -> push (Mul (convert a, convert b))
      | Div (a, b) -> push (Div (convert a, convert b))
      | Mod (a, b) -> push (Mod (convert a, convert b))
      | LShift (a, b) -> push (LShift (convert a, convert b))
      | RShift (a, b) -> push (RShift (convert a, convert b))
      | BitAnd (a, b) -> push (BitAnd (convert a, convert b))
      | BitOr (a, b) -> push (BitOr (convert a, convert b))
      | BitXor (a, b) -> push (BitXor (convert a, convert b))
      | Eq0 a -> push (Eq0 (convert a))
      | Eq (a, b) -> push (Eq (convert a, convert b))
      | Ne (a, b) -> push (Ne (convert a, convert b))
      | Lt (a, b) -> push (Lt (convert a, convert b))
      | Le (a, b) -> push (Le (convert a, convert b))
      | Gt (a, b) -> push (Gt (convert a, convert b))
      | Ge (a, b) -> push (Ge (convert a, convert b))
      | Clip (a, bits) -> push (Clip (convert a, bits))
      | Concat l ->
          let l = List.map l ~f:(fun (e, bits) -> (convert e, bits)) in
          push (Concat l)
      | Log2OneHot a -> push (Log2OneHot (convert a))
    in
    let e = convert expr in
    push' (Return e);
    { Inner.Expr.ns = Vec.to_array ns }
  in
  let convert_expr expr = convert_expr' expr in

  let chan_id_pool = Chan_id_pool.create () in
  let get_chan chan_id = Chan_id_pool.get_id chan_id_pool chan_id in

  let mem_id_pool = Mem_id_pool.create () in
  let get_mem mem = Mem_id_pool.get_id mem_id_pool var_id_pool mem in

  let rec convert_stmt stmt =
    let convert' stmt = ignore (convert_stmt stmt : Inner.Instr_idx.t) in
    let push_branches ~tag stmts =
      let split = push_instr ~tag Nop in
      let ends =
        List.map stmts ~f:(fun stmt ->
            convert' stmt;
            push_instr ~tag (Jump Inner.Instr_idx.dummy_val))
      in
      let starts =
        List.take (split :: ends) (List.length stmts)
        |> List.map ~f:Inner.Instr_idx.next
      in
      let merge = push_instr ~tag Nop in
      List.iter ends ~f:(fun end_ -> edit_instr ~tag end_ (Jump merge));
      (split, starts, merge)
    in
    let push_select_probes ~tag branches =
      let split, starts, merge =
        let split = push_instr ~tag Nop in
        let starts, ends =
          List.map branches ~f:(fun (probe, stmt) ->
              let other_probes =
                List.map branches ~f:fst
                |> List.filter ~f:(fun o -> not (Inner.Probe.equal o probe))
              in
              let start =
                push_instr ~tag
                  (SelectProbes_AssertStable (probe, other_probes))
              in
              convert' stmt;
              let end_ = push_instr ~tag (Jump Inner.Instr_idx.dummy_val) in
              (start, end_))
          |> List.unzip
        in
        let merge = push_instr ~tag Nop in
        List.iter ends ~f:(fun end_ -> edit_instr ~tag end_ (Jump merge));
        (split, starts, merge)
      in
      let guards =
        List.zip_exn branches starts
        |> List.map ~f:(fun ((probe, _), start) -> (probe, start))
      in
      edit_instr ~tag split (SelectProbes guards);
      merge
    in
    match stmt with
    | Stmt.Assign (tag, var, expr) ->
        push_instr ~tag (Assign (convert_id var, convert_expr expr))
    | Nop tag -> push_instr ~tag Nop
    | Log1 (tag, expr) -> push_instr ~tag (Log1 (convert_expr expr))
    | Assert (tag, expr, log_e) ->
        let expr = convert_expr expr in
        let log_e = convert_expr log_e in
        push_instr ~tag (Assert (expr, log_e))
    | Seq (tag, stmts) -> (
        match stmts with
        | [] -> push_instr ~tag Nop
        | stmts -> List.map stmts ~f:convert_stmt |> List.last_exn)
    | Par (tag, stmts) -> (
        match stmts with
        | [] -> push_instr ~tag Nop
        | stmts ->
            let split, starts, merge = push_branches ~tag stmts in
            edit_instr ~tag split (Par starts);
            edit_instr ~tag merge
              (ParJoin (Inner.Par_join.create ~max_ct:(List.length stmts)));
            merge)
    | SelectImm (tag, branches, else_) ->
        let guards, stmts = List.unzip branches in
        let split, starts, merge = push_branches ~tag (else_ :: stmts) in
        let guards = List.map guards ~f:convert_expr in
        let guards = List.zip_exn guards (List.tl_exn starts) in
        edit_instr ~tag split (SelectImmElse (guards, List.hd_exn starts));
        merge
    | Read (tag, chan, var) ->
        let chan_idx = get_chan chan in
        push_instr ~tag (Read (convert_id var, chan_idx))
    | Send (tag, chan, expr) ->
        let chan_idx = get_chan chan in
        push_instr ~tag (Send (convert_expr expr, chan_idx))
    | WhileLoop (tag, expr, stmt) ->
        let split =
          push_instr ~tag
            (JumpIfFalse (convert_expr expr, Inner.Instr_idx.dummy_val))
        in
        convert' stmt;
        let jmp = push_instr ~tag (Jump split) in
        edit_instr ~tag split
          (JumpIfFalse (convert_expr expr, Inner.Instr_idx.next jmp));
        jmp
    | DoWhile (tag, stmt, expr) ->
        let top = push_instr ~tag Nop in
        convert' stmt;
        let not_expr = Ir.Expr.Eq (expr, Const CInt.zero) in
        push_instr ~tag (JumpIfFalse (convert_expr' not_expr, top))
    | ReadMem (tag, mem, idx, dst) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr ~tag
          (ReadMem (convert_expr idx, convert_id dst, mem_idx_reg, mem_id))
    | WriteMem (tag, mem, idx, value) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr ~tag
          (WriteMem (convert_expr idx, convert_expr value, mem_idx_reg, mem_id))
    | Nondeterm_select (tag, branches) ->
        let branches =
          List.map branches ~f:(fun (probe, stmt) ->
              let probe =
                match probe with
                | Read chan -> Inner.Probe.Read_ready (get_chan chan)
                | Send chan -> Send_ready (get_chan chan)
              in
              (probe, stmt))
        in
        push_select_probes ~tag branches
  in

  (* Build the main program. An initial jump is required. *)
  let () =
    let init_jump =
      push_instr ~tag:Tag.dummy (Jump Inner.Instr_idx.dummy_val)
    in
    let start = Assem_builder.next_idx ab in
    edit_instr ~tag:Tag.dummy init_jump (Jump start)
  in
  let (_ : Inner.Instr_idx.t) = convert_stmt ir in
  let (_ : Inner.Instr_idx.t) = push_instr ~tag:Tag.dummy End in

  (* set up user enqueuers *)
  let all_dequeuers, dequeuer_table =
    Set.to_list user_readable_ports
    |> List.mapi ~f:(fun dequeuer_idx chan ->
           let chan_idx = Chan_id_pool.get_id chan_id_pool chan in
           let bitwidth = chan.bitwidth in
           let var_id =
             Var_id_pool.new_id var_id_pool (Read_deq_reg bitwidth)
           in
           let read_instr =
             push_instr ~tag:Tag.dummy (Read (var_id, chan_idx))
           in
           let _ = push_instr ~tag:Tag.dummy (Read_dequeuer dequeuer_idx) in
           let sexper = CInt.sexp_of_t in
           (* let dequeuer = Dequeuer_buff.create ~var_id chan_idx in *)
           ((chan, (read_instr, dequeuer_idx)), (chan, sexper, var_id, chan_idx)))
    |> List.unzip
  in
  let all_dequeuers = Chan.Map.of_alist_exn all_dequeuers in
  let dequeuers, dequeuer_table_info =
    List.map dequeuer_table ~f:(fun (chan, sexper, var_id, inner_chan) ->
        ( { Inner.Dequeuer_spec.var_id },
          { Dequeuer_info.inner_chan; chan; sexper } ))
    |> Array.of_list |> Array.unzip
  in

  (* set up user dequeuers *)
  let all_enqueuers, enqueuer_table =
    Set.to_list user_sendable_ports
    |> List.mapi ~f:(fun enqueuer_idx chan ->
           let chan_idx = Chan_id_pool.get_id chan_id_pool chan in
           let bitwidth = chan.bitwidth in
           let var_id =
             Var_id_pool.new_id var_id_pool (Send_enq_reg bitwidth)
           in
           let send_expr = { Inner.Expr.ns = [| Var var_id; Return 0 |] } in
           let send_instr =
             push_instr ~tag:Tag.dummy (Send (send_expr, chan_idx))
           in
           let _ = push_instr ~tag:Tag.dummy (Send_enqueuer enqueuer_idx) in
           (* let enqueuer = in *)
           ((chan, (send_instr, enqueuer_idx)), (chan, var_id, chan_idx)))
    |> List.unzip
  in
  let all_enqueuers = Chan.Map.of_alist_exn all_enqueuers in
  let enqueuers, enqueuer_table_info =
    List.map enqueuer_table ~f:(fun (chan, var_id, inner_chan) ->
        ({ Inner.Enqueuer_spec.var_id }, { Enqueuer_info.inner_chan; chan }))
    |> Array.of_list |> Array.unzip
  in

  let assem = Assem_builder.assem_array ab in
  let assem, l = Array.unzip assem in
  let tag_of_assem_idx = l in
  let assem_guard_read_ids =
    Array.map assem ~f:(fun n -> inner_n_get_read_ids n)
  in
  let assem_guard_write_ids =
    Array.map assem ~f:(fun n -> inner_n_get_write_ids n)
  in
  let vars, var_table_info =
    let var_srcs =
      Hashtbl.to_alist var_id_pool.src_of_id
      |> List.sort ~compare:(fun (id1, _) (id2, _) -> Int.compare id1 id2)
    in
    List.iteri var_srcs ~f:(fun i (id, _) -> assert (Int.equal i id));
    List.map var_srcs ~f:(fun (_, src) ->
        let bitwidth = Var_id_src.bitwidth src in
        let spec = { Inner.Var_spec.bitwidth } in
        let info = { Var_buff_info.src } in
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
        let bitwidth = chan.bitwidth in
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
        let init = mem.init in
        let cell_bitwidth = mem.cell_bitwidth in
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
    tag_of_assem_idx;
    var_table_info;
    chan_table_info;
    enqueuer_table_info;
    dequeuer_table_info;
    mem_table_info;
    all_enqueuers;
    all_dequeuers;
  }

let reset t =
  Inner.reset t.i;
  t.is_done <- false

let create ~seed stmt ~iports ~oports =
  let user_sendable_ports = Chan.Set.of_list iports in
  let user_readable_ports = Chan.Set.of_list oports in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
  let stmt = flatten stmt in
  (* print_s [%sexp (stmt: Stmt.t)]; *)
  let t = create_t ~seed stmt ~user_sendable_ports ~user_readable_ports in
  reset t;
  t
