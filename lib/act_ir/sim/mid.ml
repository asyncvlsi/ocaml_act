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

module RW = struct
  module T = struct
    type t = Var of Var.t | Mem of Mem.t
    [@@deriving sexp, hash, compare, equal]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module IStmt = struct
  type n =
    | Nop
    | Assign of Var.t * Var.t Expr.t
    | Log1 of Var.t Expr.t
    | Assert of Var.t Expr.t * Var.t Expr.t
    | Seq of t list
    | Par of t list
    | SelectImm of (Var.t Expr.t * t) list * t
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t Expr.t
    | WhileLoop of Var.t Expr.t * t
    | DoWhile of t * Var.t Expr.t
    | ReadMem of Mem.t * Var.t Expr.t * Var.t
    | WriteMem of Mem.t * Var.t Expr.t * Var.t Expr.t
    | Nondeterm_select of (Probe.t * t) list

  and t = { n : n; tag : Tag.t; writes : RW.t list; reads : Var.t list }
  [@@deriving sexp_of]

  let rec of_stmt stmt =
    match stmt with
    | Stmt.Nop tag -> { n = Nop; tag; writes = []; reads = [] }
    | Assign (tag, var, expr) ->
        {
          n = Assign (var, expr);
          tag;
          writes = [ Var var ];
          reads = Expr.var_ids expr;
        }
    | Log1 (tag, expr) ->
        { n = Log1 expr; tag; writes = []; reads = Expr.var_ids expr }
    | Assert (tag, e1, e2) ->
        {
          n = Assert (e1, e2);
          tag;
          writes = [];
          reads = Expr.var_ids e1 @ Expr.var_ids e2;
        }
    | Seq (tag, ns) ->
        { n = Seq (List.map ~f:of_stmt ns); tag; writes = []; reads = [] }
    | Par (tag, ns) ->
        { n = Par (List.map ~f:of_stmt ns); tag; writes = []; reads = [] }
    | Read (tag, chan, var) ->
        { n = Read (chan, var); tag; writes = [ Var var ]; reads = [] }
    | Send (tag, chan, expr) ->
        { n = Send (chan, expr); tag; writes = []; reads = Expr.var_ids expr }
    | ReadMem (tag, mem, expr, dst) ->
        {
          n = ReadMem (mem, expr, dst);
          tag;
          writes = [ Var dst; Mem mem ];
          reads = Expr.var_ids expr;
        }
    | WriteMem (tag, mem, expr, value) ->
        {
          n = WriteMem (mem, expr, value);
          tag;
          writes = [ Mem mem ];
          reads = Expr.var_ids expr @ Expr.var_ids value;
        }
    | SelectImm (tag, branches, else_) ->
        {
          n =
            SelectImm
              ( List.map branches ~f:(fun (g, stmt) -> (g, of_stmt stmt)),
                of_stmt else_ );
          tag;
          writes = [];
          reads = List.concat_map branches ~f:(fun (g, _) -> Expr.var_ids g);
        }
    | WhileLoop (tag, g, stmt) ->
        {
          n = WhileLoop (g, of_stmt stmt);
          tag;
          writes = [];
          reads = Expr.var_ids g;
        }
    | DoWhile (tag, stmt, g) ->
        {
          n = DoWhile (of_stmt stmt, g);
          tag;
          writes = [];
          reads = Expr.var_ids g;
        }
    | Nondeterm_select (tag, branches) ->
        {
          n =
            Nondeterm_select
              (List.map branches ~f:(fun (g, stmt) -> (g, of_stmt stmt)));
          tag;
          writes = [];
          reads = [];
        }
end

(* a few simle optimizations on the Stmt datatype to generate better byte
   code *)
let rec flatten stmt =
  let flatten_brs branches =
    List.map branches ~f:(fun (g, stmt) -> (g, flatten stmt))
  in
  let n =
    match stmt.IStmt.n with
    | Nop | Assign _ | Log1 _ | Read _ | Send _ -> `Stmt stmt
    | ReadMem _ | WriteMem _ -> `Stmt stmt
    | Assert _ -> `Stmt stmt
    | Seq ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n ->
                 match n.IStmt.n with Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n.n with Seq ns -> ns | _ -> [ n ])
        in
        match ns with
        | [] -> `N IStmt.Nop
        | [ n ] -> `Stmt n
        | ls -> `N (Seq ls))
    | Par ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n ->
                 match n.IStmt.n with Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n.n with Par ns -> ns | _ -> [ n ])
        in
        match ns with
        | [] -> `N IStmt.Nop
        | [ n ] -> `Stmt n
        | ls -> `N (Par ls))
    | SelectImm (branches, else_) ->
        `N (SelectImm (flatten_brs branches, flatten else_))
    | WhileLoop (g, stmt) -> `N (WhileLoop (g, flatten stmt))
    | DoWhile (stmt, g) -> `N (DoWhile (flatten stmt, g))
    | Nondeterm_select branches -> `N (Nondeterm_select (flatten_brs branches))
  in
  match n with
  | `Stmt stmt -> stmt
  | `N n ->
      { IStmt.n; tag = stmt.tag; reads = stmt.reads; writes = stmt.writes }

let prune_guards stmt =
  let rec h stmt =
    let stmt_reads = List.map stmt.IStmt.reads ~f:(fun v -> RW.Var v) in
    match stmt.n with
    | Nop | Assign _ | Log1 _ | Read _ | Send _ | ReadMem _ | WriteMem _
    | Assert _ ->
        (stmt_reads, stmt.writes, [])
    | Seq ns ->
        let reads, writes, needs_guard = List.map ns ~f:h |> List.unzip3 in
        (List.concat reads, List.concat writes, List.concat needs_guard)
    | SelectImm (branches, else_) ->
        let reads, writes, needs_guard =
          List.map branches ~f:(fun (_, n) -> h n) |> List.unzip3
        in
        let else_reads, else_writes, else_needs_guard = h else_ in
        let reads = List.concat ([ stmt_reads; else_reads ] @ reads) in
        let writes = List.concat ([ stmt.writes; else_writes ] @ writes) in
        let needs_guard = List.concat (else_needs_guard :: needs_guard) in
        (reads, writes, needs_guard)
    | WhileLoop (_, stmt) | DoWhile (stmt, _) ->
        let reads, writes, needs_guard = h stmt in
        (reads @ stmt_reads, writes @ stmt.writes, needs_guard)
    | Nondeterm_select branches ->
        let reads, writes, needs_guard =
          List.map branches ~f:(fun (_, n) -> h n) |> List.unzip3
        in
        (List.concat reads, List.concat writes, List.concat needs_guard)
    | Par ns ->
        let nns = List.map ns ~f:h in
        let ns = List.to_array nns in
        let inter =
          List.init (Array.length ns) ~f:Fn.id
          |> List.concat_map ~f:(fun i ->
                 if i <= 0 then []
                 else
                   List.init (i - 1) ~f:Fn.id
                   |> List.concat_map ~f:(fun j ->
                          let i_reads, i_writes, _ = ns.(i) in
                          let j_reads, j_writes, _ = ns.(j) in
                          let i_reads = RW.Set.of_list i_reads in
                          let i_writes = RW.Set.of_list i_writes in
                          let j_reads = RW.Set.of_list j_reads in
                          let j_writes = RW.Set.of_list j_writes in
                          let inter =
                            Set.union
                              (Set.inter i_reads j_writes)
                              (Set.inter j_reads i_writes)
                            |> Set.union (Set.inter i_writes j_writes)
                          in
                          Set.to_list inter))
        in
        let reads, writes, needs_guard = List.unzip3 nns in
        (List.concat reads, List.concat writes, inter @ List.concat needs_guard)
  in
  let _, _, needs_guard = h stmt in
  let needs_guard = RW.Set.of_list needs_guard in

  let rec h stmt =
    let h_brs branches = List.map branches ~f:(fun (g, stmt) -> (g, h stmt)) in
    let n =
      match stmt.IStmt.n with
      | Nop -> IStmt.Nop
      | Assign (var, expr) -> Assign (var, expr)
      | Log1 expr -> Log1 expr
      | Read (chan, var) -> Read (chan, var)
      | Send (chan, expr) -> Send (chan, expr)
      | ReadMem (mem, idx, dst) -> ReadMem (mem, idx, dst)
      | WriteMem (mem, idx, value) -> WriteMem (mem, idx, value)
      | Assert (e1, e2) -> Assert (e1, e2)
      | Seq ns -> Seq (List.map ns ~f:h)
      | Par ns -> Par (List.map ns ~f:h)
      | SelectImm (branches, else_) -> SelectImm (h_brs branches, h else_)
      | WhileLoop (g, stmt) -> WhileLoop (g, h stmt)
      | DoWhile (stmt, g) -> DoWhile (h stmt, g)
      | Nondeterm_select branches -> Nondeterm_select (h_brs branches)
    in
    let reads =
      List.filter stmt.reads ~f:(fun v -> Set.mem needs_guard (Var v))
    in
    let writes = List.filter stmt.writes ~f:(Set.mem needs_guard) in
    { IStmt.n; tag = stmt.tag; reads; writes }
  in
  h stmt

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
  type t = {
    assem :
      (Inner.N.t * (Tag.t * (Inner.Var_id.t list * Inner.Var_id.t list))) Vec.t;
  }

  let create () =
    { assem = Vec.create ~cap:10 ~default:(Inner.N.End, (Tag.dummy, ([], []))) }

  let push t ~tag ~reads ~writes (instr : Inner.N.t) =
    Vec.push t.assem (instr, (tag, (reads, writes)));
    Vec.length t.assem - 1

  let edit t ~tag ~reads ~writes idx (instr : Inner.N.t) =
    Vec.set t.assem idx (instr, (tag, (reads, writes)))

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

let create_t ~seed (ir : IStmt.t) ~user_sendable_ports ~user_readable_ports =
  let ab = Assem_builder.create () in
  let push_instr ~tag ~reads ~writes instr =
    Assem_builder.push ab ~tag ~reads ~writes instr
  in
  let edit_instr ~tag ~reads ~writes idx instr =
    Assem_builder.edit ab ~tag ~reads ~writes idx instr
  in

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

  let rec convert_stmt (stmt : IStmt.t) =
    let convert' stmt = ignore (convert_stmt stmt : Inner.Instr_idx.t) in
    let push_branches ~tag stmts =
      let split = push_instr ~tag ~reads:[] ~writes:[] Nop in
      let ends =
        List.map stmts ~f:(fun stmt ->
            convert' stmt;
            push_instr ~tag ~reads:[] ~writes:[]
              (Jump Inner.Instr_idx.dummy_val))
      in
      let starts =
        List.take (split :: ends) (List.length stmts)
        |> List.map ~f:Inner.Instr_idx.next
      in
      let merge = push_instr ~tag ~reads:[] ~writes:[] Nop in
      List.iter ends ~f:(fun end_ ->
          edit_instr ~tag ~reads:[] ~writes:[] end_ (Jump merge));
      (split, starts, merge)
    in
    let { IStmt.tag; n; reads; writes } = stmt in
    let reads = List.map reads ~f:convert_id in
    let writes =
      List.map writes ~f:(fun id ->
          match id with
          | Var var -> convert_id var
          | Mem mem ->
              let mem_idx_reg, _mem_id = get_mem mem in
              mem_idx_reg)
    in
    match n with
    | IStmt.Assign (var, expr) ->
        push_instr ~tag ~reads ~writes
          (Assign (convert_id var, convert_expr expr))
    | Nop -> push_instr ~tag ~reads ~writes Nop
    | Log1 expr -> push_instr ~tag ~reads ~writes (Log1 (convert_expr expr))
    | Assert (expr, log_e) ->
        let expr = convert_expr expr in
        let log_e = convert_expr log_e in
        push_instr ~tag ~reads ~writes (Assert (expr, log_e))
    | Seq stmts -> (
        match stmts with
        | [] -> failwith "unreachable: flatten should remove this"
        | stmts -> List.map stmts ~f:convert_stmt |> List.last_exn)
    | Par stmts -> (
        match stmts with
        | [] -> failwith "unreachable: flatten should remove this"
        | stmts ->
            let split, starts, merge = push_branches ~tag stmts in
            edit_instr ~tag ~reads ~writes split (Par starts);
            edit_instr ~tag ~reads:[] ~writes:[] merge
              (ParJoin (Inner.Par_join.create ~max_ct:(List.length stmts)));
            merge)
    | SelectImm (branches, else_) ->
        let guards, stmts = List.unzip branches in
        let split, starts, merge = push_branches ~tag (else_ :: stmts) in
        let guards = List.map guards ~f:convert_expr in
        let guards = List.zip_exn guards (List.tl_exn starts) in
        edit_instr ~tag ~reads ~writes split
          (SelectImmElse (guards, List.hd_exn starts));
        merge
    | Read (chan, var) ->
        let chan_idx = get_chan chan in
        push_instr ~tag ~reads ~writes (Read (convert_id var, chan_idx))
    | Send (chan, expr) ->
        let chan_idx = get_chan chan in
        push_instr ~tag ~reads ~writes (Send (convert_expr expr, chan_idx))
    | WhileLoop (expr, stmt) ->
        let split =
          push_instr ~tag ~reads ~writes
            (JumpIfFalse (convert_expr expr, Inner.Instr_idx.dummy_val))
        in
        convert' stmt;
        let jmp = push_instr ~tag ~reads:[] ~writes:[] (Jump split) in
        edit_instr ~tag ~reads ~writes split
          (JumpIfFalse (convert_expr expr, Inner.Instr_idx.next jmp));
        jmp
    | DoWhile (stmt, expr) ->
        let top = push_instr ~tag ~reads:[] ~writes:[] Nop in
        convert' stmt;
        let not_expr = Ir.Expr.Eq (expr, Const CInt.zero) in
        push_instr ~tag ~reads ~writes
          (JumpIfFalse (convert_expr' not_expr, top))
    | ReadMem (mem, idx, dst) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr ~tag ~reads ~writes
          (ReadMem (convert_expr idx, convert_id dst, mem_idx_reg, mem_id))
    | WriteMem (mem, idx, value) ->
        let mem_idx_reg, mem_id = get_mem mem in
        push_instr ~tag ~reads ~writes
          (WriteMem (convert_expr idx, convert_expr value, mem_idx_reg, mem_id))
    | Nondeterm_select branches ->
        let branches =
          List.map branches ~f:(fun (probe, stmt) ->
              let probe =
                match probe with
                | Read chan -> Inner.Probe.Read_ready (get_chan chan)
                | Send chan -> Send_ready (get_chan chan)
              in
              (probe, stmt))
        in
        let split, starts, merge =
          let split = push_instr ~tag ~reads ~writes Nop in
          let starts, ends =
            List.map branches ~f:(fun (probe, stmt) ->
                let other_probes =
                  List.map branches ~f:fst
                  |> List.filter ~f:(fun o -> not (Inner.Probe.equal o probe))
                in
                let start =
                  push_instr ~tag ~reads:[] ~writes:[]
                    (SelectProbes_AssertStable (probe, other_probes))
                in
                convert' stmt;
                let end_ =
                  push_instr ~tag ~reads:[] ~writes:[]
                    (Jump Inner.Instr_idx.dummy_val)
                in
                (start, end_))
            |> List.unzip
          in
          let merge = push_instr ~tag ~reads:[] ~writes:[] Nop in
          List.iter ends ~f:(fun end_ ->
              edit_instr ~tag ~reads:[] ~writes:[] end_ (Jump merge));
          (split, starts, merge)
        in
        let guards =
          List.zip_exn branches starts
          |> List.map ~f:(fun ((probe, _), start) -> (probe, start))
        in
        edit_instr ~tag ~reads ~writes split (SelectProbes guards);
        merge
  in

  (* Build the main program. An initial jump is required. *)
  let () =
    let init_jump =
      push_instr ~tag:Tag.dummy ~reads:[] ~writes:[]
        (Jump Inner.Instr_idx.dummy_val)
    in
    let start = Assem_builder.next_idx ab in
    edit_instr ~tag:Tag.dummy ~reads:[] ~writes:[] init_jump (Jump start)
  in
  let (_ : Inner.Instr_idx.t) = convert_stmt ir in
  let (_ : Inner.Instr_idx.t) =
    push_instr ~tag:Tag.dummy ~reads:[] ~writes:[] End
  in

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
             push_instr ~tag:Tag.dummy ~reads:[] ~writes:[ var_id ]
               (Read (var_id, chan_idx))
           in
           let _ =
             push_instr ~tag:Tag.dummy ~reads:[] ~writes:[]
               (Read_dequeuer dequeuer_idx)
           in
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
             push_instr ~tag:Tag.dummy ~reads:[ var_id ] ~writes:[]
               (Send (send_expr, chan_idx))
           in
           let _ =
             push_instr ~tag:Tag.dummy ~reads:[] ~writes:[]
               (Send_enqueuer enqueuer_idx)
           in
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
  let tag_of_assem_idx, read_writes = Array.unzip l in
  let assem_guard_read_ids, assem_guard_write_ids =
    Array.map read_writes ~f:(fun (reads, writes) ->
        let reads = Inner.Var_id.Set.of_list reads in
        let writes = Inner.Var_id.Set.of_list writes in
        let reads = Set.diff reads writes in
        (reads, writes))
    |> Array.unzip
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
  let stmt = IStmt.of_stmt stmt |> flatten |> prune_guards in
  (* print_s [%sexp (stmt: IStmt.t)]; *)
  let t = create_t ~seed stmt ~user_sendable_ports ~user_readable_ports in
  reset t;
  t
