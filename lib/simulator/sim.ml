open! Core
open! Act

let for_loop_else max_ct ~(f : int -> [ `Continue | `Return of 'a ])
    ~(else_ : 'a) =
  let return = ref None in
  let ct = ref 0 in
  while !ct < max_ct && Option.is_none !return do
    (match f !ct with `Continue -> () | `Return d -> return := Some d);
    incr ct
  done;
  Option.value !return ~default:else_

module Ir = Internal_rep

let some_or_thunk o ~f = match o with Some v -> Some v | None -> f ()

module Var_id = Int

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

module With_origin = struct
  type 'a t = { value : 'a; origin : Code_pos.t } [@@deriving sexp_of]

  let map { value; origin } ~f = { value = f value; origin }
end

module Instr_idx = struct
  include Int

  let dummy_val = -1
  let next i = i + 1
end

module Send_enqueuer = struct
  type t = {
    mutable to_send : Any.t With_origin.t array;
    var_id : Var_id.t;
    mutable is_done : bool;
    mutable idx : int;
    ir_chan : (Chan.Ir.U.t[@sexp.opaque]);
  }
  [@@deriving sexp_of]

  let create ~var_id ir_chan =
    { to_send = [||]; var_id; is_done = true; idx = 0; ir_chan }
end

module Read_dequeuer = struct
  type t = {
    mutable expected_reads : Any.t With_origin.t array;
    var_id : Var_id.t;
    equals : (Any.t -> Any.t -> bool[@sexp.opaque]);
    mutable idx : int;
    ir_chan : (Chan.Ir.U.t[@sexp.opaque]);
  }
  [@@deriving sexp_of]

  let create ~var_id ~equals ir_chan =
    { expected_reads = [||]; var_id; equals; idx = 0; ir_chan }
end

module Var_id_src = struct
  type t = Var of Var.Ir.U.t | Mem_idx_reg | Read_deq_reg | Send_enq_reg
end

module Var_id_pool = struct
  type t = {
    mutable next_id : int;
    var_id_of_var : Var_id.t Var.Ir.U.Table.t;
    src_of_var_id : Var_id_src.t Var_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      var_id_of_var = Ir.Var.U.Table.create ();
      src_of_var_id = Var_id.Table.create ();
    }

  let new_id t src =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.set t.src_of_var_id ~key:id ~data:src;
    id

  let to_assem_id t var =
    Hashtbl.find_or_add t.var_id_of_var var ~default:(fun () ->
        new_id t (Var_id_src.Var var))

  let get_src_exn t var_id : Var_id_src.t =
    Hashtbl.find_exn t.src_of_var_id var_id

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

  let var_inits t = t.src_of_var_id
end

module Chan_id = Int

module Chan_id_pool = struct
  type t = {
    mutable next_id : int;
    id_of_chan : Chan_id.t Ir.Chan.U.Table.t;
    enq_of_id :
      (Send_enqueuer.t * (* send instr *) Instr_idx.t) Chan_id.Table.t;
    deq_of_id : (Read_dequeuer.t * (* read instr *) Instr_idx.t) Chan_id.Table.t;
  }

  let create () =
    {
      next_id = 0;
      id_of_chan = Ir.Chan.U.Table.create ();
      enq_of_id = Chan_id.Table.create ();
      deq_of_id = Chan_id.Table.create ();
    }

  let new_id t =
    t.next_id <- t.next_id + 1;
    t.next_id - 1

  let get_id t chan =
    Hashtbl.find_or_add t.id_of_chan chan ~default:(fun () -> new_id t)

  let find t chan = Hashtbl.find t.id_of_chan chan
  let find_exn t chan = Hashtbl.find_exn t.id_of_chan chan

  let get_enqueuer t chan =
    Option.map (find t chan) ~f:(fun id -> Hashtbl.find_exn t.enq_of_id id)

  let get_dequeuer t chan =
    Option.map (find t chan) ~f:(fun id -> Hashtbl.find_exn t.deq_of_id id)

  let set_dequeuer_exn t id ~dequeuer ~read_instr =
    Hashtbl.set t.deq_of_id ~key:id ~data:(dequeuer, read_instr)

  let set_enqueuer_exn t id ~enqueuer ~send_instr =
    Hashtbl.set t.enq_of_id ~key:id ~data:(enqueuer, send_instr)

  let ct t = t.next_id
end

module Chan_buff = struct
  type t = {
    mutable read_ready : bool;
    mutable read_instr : Instr_idx.t;
    mutable read_dst_var_id : Var_id.t;
    mutable send_ready : bool;
    mutable send_instr : Instr_idx.t;
    mutable send_expr : Expr.t;
    mutable waiting_on_send_ready : Instr_idx.t Vec.t;
    mutable waiting_on_read_ready : Instr_idx.t Vec.t;
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
      waiting_on_send_ready = Vec.create ~cap:10 ~default:0;
      waiting_on_read_ready = Vec.create ~cap:10 ~default:0;
    }
end

module Mem_id = Int

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
        (Var_id_pool.new_id var_id_pool Mem_idx_reg, new_id t))

  let id_of_mem t = t.id_of_mem
end

module Mem_buff = struct
  type t = { arr : Any.t array; idx_helper_reg : Var_id.t } [@@deriving sexp_of]

  let create ~init ~idx_helper_reg = { arr = Array.copy init; idx_helper_reg }
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
    | Log of Expr.t
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
        (* idx *) Expr.t
        * (* dst *) Var_id.t
        * (* idx_reg *) Var_id.t
        * Mem_id.t
    | WriteMem of
        (* idx *) Expr.t * (* src *) Expr.t * (* idx_reg *) Var_id.t * Mem_id.t
    (* These are ``magic'' instructions that allow user io operations. These instruction
       should be placed immediatly after the assoiated send/read instruction *)
    | Send_enqueuer of Send_enqueuer.t
    | Read_dequeuer of Read_dequeuer.t
    | WaitUntilReadReady of Chan_id.t
    | WaitUntilSendReady of Chan_id.t
    (* These allow for probabilistic testing that a probe is stable *)
    | AssertStillReadReady of Chan_id.t
    | AssertStillSendReady of Chan_id.t
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
    | ReadMem (idx_expr, _, _, _) -> Expr.var_ids idx_expr
    | WriteMem (idx_expr, src_expr, _, _) ->
        Expr.var_ids idx_expr @ Expr.var_ids src_expr
    | WaitUntilSendReady _ | WaitUntilReadReady _ -> []
    | AssertStillReadReady _ | AssertStillSendReady _ -> [])
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
    | ReadMem (_, dst, mem_idx_reg, _) -> [ dst; mem_idx_reg ]
    | WriteMem (_, _, mem_idx_reg, _) -> [ mem_idx_reg ]
    | WaitUntilSendReady _ | WaitUntilReadReady _ -> []
    | AssertStillReadReady _ | AssertStillSendReady _ -> [])
    |> Var_id.Set.of_list
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

  let create ~var_id_pool =
    let var_inits = Var_id_pool.var_inits var_id_pool in
    let var_ct =
      (Hashtbl.keys var_inits
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0)
      + 1
    in
    let values = Array.init var_ct ~f:(fun _ -> Obj.magic 0) in
    let is_inited = Array.init var_ct ~f:(fun _ -> false) in
    Hashtbl.iteri var_inits ~f:(fun ~key:var_id ~data:kind ->
        match kind with
        | Var_id_src.Var var -> (
            match var.Var.Ir.U.d.init with
            | Some value ->
                values.(var_id) <- value;
                is_inited.(var_id) <- true
            | None -> is_inited.(var_id) <- false)
        | Mem_idx_reg | Read_deq_reg | Send_enq_reg -> ());
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
  all_enqueuers : (Instr_idx.t * Send_enqueuer.t) list;
  all_dequeuers : (Instr_idx.t * Read_dequeuer.t) list;
  var_id_pool : Var_id_pool.t;
  chan_id_pool : Chan_id_pool.t;
  mem_id_pool : Mem_id_pool.t;
  (* per-wait state *)
  mutable queued_user_ops : Queued_user_op.t Queue.t;
  (* simulation state *)
  mutable pcs : Instr_idx.t Vec.t;
  mutable var_table : Var_table.t;
  mutable chan_table : Chan_buff.t array;
  mutable mem_table : Mem_buff.t array;
  mutable rng : (Random.State.t[@sexp.opaque]);
  mutable is_done : bool;
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
      unguard pc;
      assert (not d.is_done);
      if d.idx >= Array.length d.to_send then (
        Vec.remove t.pcs pc_idx;
        d.is_done <- true;
        Ok ())
      else
        let { With_origin.value; origin = _ } = d.to_send.(d.idx) in
        d.idx <- d.idx + 1;
        Var_table.set t.var_table ~var_id:d.var_id ~value;
        set_pc_and_guard ~pc_idx (pc - 1)
  | Read_dequeuer d ->
      unguard pc;
      let value = Var_table.at t.var_table ~var_id:d.var_id in
      let expected = d.expected_reads.(d.idx) in
      if not (d.equals value expected.value) then
        let value = d.ir_chan.d.dtype.sexp_of_t value in
        let expected =
          With_origin.map expected ~f:d.ir_chan.d.dtype.sexp_of_t
        in
        Error
          (`Read_dequeuer_wrong_value (pc, d.ir_chan, value, expected, d.idx))
      else (
        d.idx <- d.idx + 1;
        if d.idx >= Array.length d.expected_reads then (
          Vec.remove t.pcs pc_idx;
          Ok ())
        else set_pc_and_guard ~pc_idx (pc - 1))
  | ReadMem (idx_expr, dst_id, _, mem_id) ->
      unguard pc;
      let mem = t.mem_table.(mem_id) in
      let idx = Var_table.eval_int t.var_table idx_expr in
      if idx < 0 || idx >= Array.length mem.arr then
        Error (`Mem_out_of_bounds (pc, idx, Array.length mem.arr))
      else (
        Var_table.set t.var_table ~var_id:dst_id ~value:mem.arr.(idx);
        set_pc_and_guard ~pc_idx (pc + 1))
  | WriteMem (idx_expr, src_expr, _, mem_id) ->
      unguard pc;
      let mem = t.mem_table.(mem_id) in
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
      match Var_id_pool.get_src_exn t.var_id_pool var_id with
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
  | `Stuck -> (
      (* check whether we need error because the queuers are unfinished *)
      match
        List.find t.all_dequeuers ~f:(fun (_, d) ->
            not (Int.equal d.idx (Array.length d.expected_reads)))
      with
      | Some (_, deq) ->
          let ir_chan = deq.ir_chan.d.creation_code_pos in
          let expected = deq.expected_reads.(deq.idx) in
          Error
            [%string
              "User read did not complete:  called %{str_l expected.origin}, \
               on chan created %{str_l ir_chan}."]
      | None -> (
          match List.find t.all_enqueuers ~f:(fun (_, d) -> not d.is_done) with
          | Some (_, enq) ->
              let ir_chan = enq.ir_chan.d.creation_code_pos in
              let expected = enq.to_send.(enq.idx - 1) in
              Error
                [%string
                  "User send did not complete:  called %{str_l \
                   expected.origin}, on chan created %{str_l ir_chan}."]
          | None -> Ok `Stuck))
  | `Uninit_id (var_id, pc) ->
      let var =
        match Var_id_pool.get_src_exn t.var_id_pool var_id with
        | Var var -> var
        | Mem_idx_reg | Read_deq_reg | Send_enq_reg ->
            failwith "should be unreachable"
      in
      Error
        [%string
          "Uninitialized variable: read %{str_i pc}, created %{str_l \
           var.d.creation_code_pos}."]
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
      match Var_id_pool.get_src_exn t.var_id_pool var_id with
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
      | Read_deq_reg | Send_enq_reg -> failwith "should not be possible")
  | `Select_no_guards_true pc ->
      Error [%string "Select statement has no true guards: %{str_i pc}."]
  | `Select_multiple_guards_true (pc, branch_idxs) ->
      let branch_idxs = List.to_string ~f:Int.to_string branch_idxs in
      Error
        [%string
          "Select statement has multiple true guards: %{str_i pc}, true branch \
           indices as %{branch_idxs}."]
  | `Read_dequeuer_wrong_value (_, ir_chan, actual, expected, _) ->
      let chan_decl = ir_chan.Chan.Ir.U.d.creation_code_pos in
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
    assert (Result.is_ok (Var_table.guard' t.var_table t.assem (send_instr + 1)));
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
    assert (Result.is_ok (Var_table.guard' t.var_table t.assem read_instr));
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
    | Log (loc, expr) -> push_instr loc (Log (convert_expr expr))
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
    let var_id = Var_id_pool.new_id var_id_pool Read_deq_reg in
    let read_instr = push_instr Code_pos.dummy_loc (Read (var_id, chan_idx)) in
    let dequeuer =
      Read_dequeuer.create ~var_id ~equals:chan.d.dtype.equal chan
    in
    let _ = push_instr Code_pos.dummy_loc (Read_dequeuer dequeuer) in
    Chan_id_pool.set_dequeuer_exn chan_id_pool chan_idx ~dequeuer ~read_instr
  in
  Set.iter user_readable_ports ~f:push_dequeuer;

  (* set up user dequeuers *)
  let push_enqueuer chan =
    let chan_idx = Chan_id_pool.find_exn chan_id_pool chan in
    let var_id = Var_id_pool.new_id var_id_pool Send_enq_reg in
    let send_instr =
      push_instr Code_pos.dummy_loc (Send (Var var_id, chan_idx))
    in
    let enqueuer = Send_enqueuer.create ~var_id chan in
    let _ = push_instr Code_pos.dummy_loc (Send_enqueuer enqueuer) in
    Chan_id_pool.set_enqueuer_exn chan_id_pool chan_idx ~enqueuer ~send_instr
  in
  Set.iter user_sendable_ports ~f:push_enqueuer;

  let assem = Assem_builder.assem_array ab in
  let assem, loc_of_assem_idx = Array.unzip assem in
  let var_table = Var_table.create ~var_id_pool in
  let pcs = Vec.of_array [| 0 |] ~default:(-1) in
  let rng = Random.State.make [| seed |] in
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
  let chan_table =
    Array.init (Chan_id_pool.ct chan_id_pool) ~f:(fun _ -> Chan_buff.create ())
  in
  let mem_table =
    let tbl =
      Mem_id_pool.id_of_mem mem_id_pool
      |> Hashtbl.to_alist
      |> List.map ~f:(fun (mem, (idx_reg, mem_id)) -> (mem_id, (idx_reg, mem)))
    in
    List.iteri tbl ~f:(fun i (mem_id, _) -> assert (Int.equal i mem_id));
    List.map tbl ~f:snd
    |> List.map ~f:(fun (idx_helper_reg, mem) ->
           Mem_buff.create ~init:(Array.copy mem.d.init) ~idx_helper_reg)
    |> Array.of_list
  in
  {
    assem;
    loc_of_assem_idx;
    all_enqueuers;
    all_dequeuers;
    var_id_pool;
    chan_id_pool;
    mem_id_pool;
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
  let call_site = Code_pos.value_or_psite loc in
  match Chan_id_pool.get_enqueuer t.chan_id_pool chan with
  | Some (enqueuer, send_instr) ->
      Queue.enqueue t.queued_user_ops
        {
          Queued_user_op.queuer = `Send enqueuer;
          chan_instr = send_instr;
          value = Any.of_magic value;
          call_site;
        }
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-sendable chan in \
         Sim.create"

let read t ?loc chan value =
  let chan = Ir.Chan.unwrap_r chan in
  let call_site = Code_pos.value_or_psite loc in
  match Chan_id_pool.get_dequeuer t.chan_id_pool chan with
  | Some (dequeuer, read_instr) ->
      Queue.enqueue t.queued_user_ops
        {
          Queued_user_op.queuer = `Read dequeuer;
          chan_instr = read_instr;
          value = Any.of_magic value;
          call_site;
        }
  | None ->
      failwith
        "the provided chan_id was not regestered as a user-readable chan in \
         Sim.create"
