open! Core
module Code_pos = Act.Code_pos
module CInt = Act.CInt

module Chp = struct
  module Var = struct
    module Id = Int

    module T = struct
      type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
      [@@deriving sexp, hash, equal, compare]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let bitwidth v = v.bitwidth
  end

  module Chan = struct
    module Id = Int

    module T = struct
      type t = { id : Id.t; bitwidth : int; creation_code_pos : Act.Code_pos.t }
      [@@deriving sexp, hash, equal, compare]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)
  end

  module Assert = struct
    module Err_msg = struct
      type t =
        | Eval_expr_failed of (Code_pos.t * (CInt.t -> string))
        | Assert_failed of Code_pos.t
        | Value_doesnt_fit of (Code_pos.t * [ `Var | `Chan | `Mem_cell ])
        | Idx_out_of_bounds of (Code_pos.t * int)
      [@@deriving sexp_of]
    end

    type t = { cond : Var.t Expr.t; log_e : Var.t Expr.t; msg : Err_msg.t }
    [@@deriving sexp_of]
  end

  module Stmt = struct
    type t =
      | Nop
      | Log of Code_pos.t * Var.t Expr.t * (CInt.t -> string)
      | Assert of Assert.t
      | Assign of Code_pos.t * Var.t * Var.t Expr.t
      | Seq of t list
      | Par of t list
      (* assert happens immediatly after read before any other code runs *)
      | ReadThenAssert of Code_pos.t * Chan.t * Var.t * Assert.t
      | Send of Code_pos.t * Chan.t * Var.t Expr.t
      | DoWhile of Code_pos.t * t * Var.t Expr.t
        (* This expr is a one-hot vector with List.length branches bits
            indexing into the list of branches *)
      | SelectImm of Code_pos.t * Var.t Expr.t * t list
    [@@deriving sexp_of]
  end

  module Proc = struct
    type t = {
      dflowable : bool;
      stmt : Stmt.t;
      iports : (Interproc_chan.t * Chan.t) list;
      oports : (Interproc_chan.t * Chan.t) list;
    }
    [@@deriving sexp_of]
  end
end

module Mem_proc = struct
  type t = {
    init : CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Interproc_chan.t;
    read_chan : Interproc_chan.t;
    write_chan : Interproc_chan.t option;
  }
  [@@deriving sexp_of]
end

module Process = struct
  module K = struct
    type t = Chp of Chp.Proc.t | Mem of Mem_proc.t [@@deriving sexp_of]
  end

  type t = { k : K.t } [@@deriving sexp_of]
end

module Program = struct
  type t = {
    processes : Process.t list;
    top_iports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
    top_oports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
  }
  [@@deriving sexp_of]
end

module Mem_with_proc_id = struct
  module T = struct
    type t = { mem : (Act.Internal_rep.Mem.t[@sexp.opaque]); proc_id : int }
    [@@deriving hash, sexp, compare, equal]
  end

  include Hashable.Make (T)
  include Comparable.Make (T)
  include T
end

let of_chp (proc : Act.Internal_rep.Chp.t) ~new_interproc_chan
    ~interproc_chan_of_ir_chan ~dflowable =
  let module Ir = Act.Internal_rep in
  let next_v_id = ref 0 in
  let var_of_var = Ir.Var.U.Table.create () in
  let new_var creation_code_pos bitwidth =
    let id = !next_v_id in
    incr next_v_id;
    { Chp.Var.id; bitwidth; creation_code_pos }
  in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout v.d.dtype with Bits_fixed bitwidth -> bitwidth
        in
        new_var v.d.creation_code_pos bitwidth)
  in

  let next_c_id = ref 0 in
  let chan_of_chan = Ir.Chan.U.Table.create () in
  let new_chan creation_code_pos bitwidth =
    let id = !next_c_id in
    incr next_c_id;
    { Chp.Chan.id; bitwidth; creation_code_pos }
  in
  let of_c c =
    Hashtbl.find_or_add chan_of_chan c ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout c.d.dtype with Bits_fixed bitwidth -> bitwidth
        in

        new_chan c.d.creation_code_pos bitwidth)
  in

  let of_e0 e loc ~of_assert ~of_var =
    let rec f e =
      match e with
      | Ir.Expr.K.Add (a, b) -> Expr.Add (f a, f b)
      | Sub_no_wrap (a, b) ->
          let a, b = (f a, f b) in
          let width_b = Expr.bitwidth b ~bits_of_var:Chp.Var.bitwidth in
          let a_concat_b =
            Expr.BitOr (LShift (a, Const (CInt.of_int width_b)), b)
          in
          let msg i =
            let b_mask = CInt.(sub (pow two (of_int width_b)) one) in
            let b = CInt.bit_and i b_mask in
            let a = CInt.(right_shift i ~amt:(of_int width_b)) in
            [%string
              "Subtraction underflowed when computing %{a#CInt} - %{b#CInt}"]
          in
          let msg = Chp.Assert.Err_msg.Eval_expr_failed (loc, msg) in
          of_assert (Expr.Ge (a, b), a_concat_b, msg);
          Sub_no_wrap (a, b)
      | Sub_wrap (a, b, bits) ->
          let p2bits =
            Expr.Const (CInt.left_shift CInt.one ~amt:(CInt.of_int bits))
          in
          let a = Expr.BitOr (Expr.Clip (f a, bits), p2bits) in
          let b = Expr.Clip (f b, bits) in
          Expr.Clip (Sub_no_wrap (a, b), bits)
      | Mul (a, b) -> Mul (f a, f b)
      | Div (a, b) -> Div (f a, f b)
      | Mod (a, b) -> Mod (f a, f b)
      | LShift (a, b) -> LShift (f a, f b)
      | LogicalRShift (a, b) -> RShift (f a, f b)
      | BitAnd (a, b) -> BitAnd (f a, f b)
      | BitOr (a, b) -> BitOr (f a, f b)
      | BitXor (a, b) -> BitXor (f a, f b)
      | Eq (a, b) -> Eq (f a, f b)
      | Ne (a, b) -> Ne (f a, f b)
      | Lt (a, b) -> Lt (f a, f b)
      | Le (a, b) -> Le (f a, f b)
      | Gt (a, b) -> Gt (f a, f b)
      | Ge (a, b) -> Ge (f a, f b)
      | Var v -> of_var v
      | Clip (e, bits) -> Clip (f e, bits)
      | Const c -> Const c
      | With_assert_log (a, v, l, ff) ->
          let a = f a in
          let v = f v in
          let l = f l in
          let msg ll = [%string "Assertion failure: %{ff ll}"] in
          let msg = Chp.Assert.Err_msg.Eval_expr_failed (loc, msg) in
          of_assert (a, l, msg);
          v
    in
    f e
  in

  let of_e e loc =
    let asserts = Queue.create () in
    let e =
      of_e0 e.Ir.Expr.k loc
        ~of_var:(fun v -> Var (of_v v))
        ~of_assert:(fun (cond, log_e, msg) ->
          Queue.enqueue asserts
            (Chp.Stmt.Assert { Chp.Assert.cond; log_e; msg }))
    in
    (e, Queue.to_list asserts)
  in

  (* This IR should encode as many runtime asserts in the tree as possible. The idea is to
      have a simple uniform representation of the program that works either for simulation
     or code generation, and so it must maintain lines of code for now. *)

  (* We pull each mem out into seperate process *)
  let mems_table = Act.Internal_rep.Mem.Table.create () in
  let chans_of_mem mem =
    Hashtbl.find_or_add mems_table mem ~default:(fun () ->
        let idx_bits = mem.d.init |> Array.length |> Int.ceil_log2 in
        let cell_bits =
          match Ir.DType.layout mem.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        (* TODO carry better error messages with these channels *)
        ( new_chan Code_pos.dummy_loc (1 + idx_bits),
          new_chan Code_pos.dummy_loc cell_bits,
          new_chan Code_pos.dummy_loc cell_bits,
          idx_bits,
          cell_bits ))
  in

  let assert_fits_cond dtype e =
    (* TODO for now ignoring asserts here *)
    let a = Act.Internal_rep.DType.of_cint_assert_expr_fn dtype in
    of_e0 a Code_pos.dummy_loc ~of_var:(fun () -> e) ~of_assert:(fun _ -> ())
  in

  let of_chp n =
    let rec of_n n =
      match n with
      | Ir.Chp.Par (_, ns) -> Chp.Stmt.Par (List.map ns ~f:of_n)
      | Seq (_, ns) -> Seq (List.map ns ~f:of_n)
      | Nop -> Nop
      | Log (loc, s) -> Log (loc, Const CInt.zero, fun _ -> s)
      | Log1 (loc, e, f) ->
          let f cint =
            Ir.Expr.Tag.value_of_cint e.tag cint |> Option.value_exn |> f
          in
          let e, asserts = of_e e loc in
          Seq [ Seq asserts; Log (loc, e, f) ]
      | Assert (loc, e) ->
          let e, asserts = of_e e loc in
          let assert_ =
            {
              Chp.Assert.cond = e;
              log_e = Const CInt.zero;
              msg = Assert_failed loc;
            }
          in
          Seq [ Seq asserts; Assert assert_ ]
      | Loop (loc, n) -> DoWhile (loc, of_n n, Expr.Const CInt.one)
      | DoWhile (loc, seq, expr) ->
          let expr, asserts = of_e expr loc in
          DoWhile (loc, Seq [ of_n seq; Seq asserts ], expr)
      | Assign (loc, id, expr) ->
          let expr, asserts = of_e expr loc in
          let assert_val_fits_in_var =
            {
              Chp.Assert.cond = assert_fits_cond id.d.dtype expr;
              log_e = expr;
              msg = Value_doesnt_fit (loc, `Var);
            }
          in
          Seq
            [
              Seq asserts;
              Assert assert_val_fits_in_var;
              Assign (loc, of_v id, expr);
            ]
      | Send (loc, chan, expr) ->
          let expr, asserts = of_e expr loc in
          let assert_val_fits_in_chan =
            {
              Chp.Assert.cond = assert_fits_cond chan.d.dtype expr;
              log_e = expr;
              msg = Value_doesnt_fit (loc, `Chan);
            }
          in
          Seq
            [
              Seq asserts;
              Assert assert_val_fits_in_chan;
              Send (loc, of_c chan, expr);
            ]
      | Read (loc, chan, var) ->
          let assert_val_fits_in_var =
            {
              Chp.Assert.cond = assert_fits_cond var.d.dtype (Var (of_v var));
              log_e = Var (of_v var);
              msg = Value_doesnt_fit (loc, `Var);
            }
          in
          ReadThenAssert (loc, of_c chan, of_v var, assert_val_fits_in_var)
      | SelectImm (loc, branches, else_) ->
          let guards =
            List.map branches ~f:(fun (guard, _) -> of_e guard loc)
          in
          let else_guard =
            match else_ with
            | Some _ ->
                let any_guard_true =
                  List.map guards ~f:fst
                  |> List.reduce ~f:(fun a b -> Expr.BitOr (a, b))
                  |> Option.value ~default:(Expr.Const CInt.zero)
                in
                [ (Expr.BitXor (any_guard_true, Const CInt.one), []) ]
            | None -> []
          in
          let guards, guard_asserts = guards @ else_guard |> List.unzip in
          let stmts =
            List.map branches ~f:snd @ Option.to_list else_ |> List.map ~f:of_n
          in
          (* TODO do this better *)
          let guard_expr =
            List.mapi guards ~f:(fun i guard ->
                Expr.LShift (guard, Const (CInt.of_int i)))
            |> List.reduce ~f:(fun a b -> Expr.BitOr (a, b))
            |> Option.value_exn
          in
          Seq
            (List.concat guard_asserts @ [ SelectImm (loc, guard_expr, stmts) ])
      | WhileLoop (loc, expr, seq) ->
          let expr, asserts = of_e expr loc in
          let guard_expr = Expr.Add (expr, Const CInt.one) in
          Seq
            [
              Seq asserts;
              SelectImm
                ( loc,
                  guard_expr,
                  [ Nop; DoWhile (loc, Seq [ of_n seq; Seq asserts ], expr) ] );
            ]
      | ReadUGMem (loc, mem, idx, dst) ->
          let idx, asserts = of_e idx loc in
          let cmd_chan, _, read_chan, _, _ = chans_of_mem mem in
          let array_len = Array.length mem.d.init in
          let assert_idx_in_bounds =
            {
              Chp.Assert.cond = Expr.(Lt (idx, Const (CInt.of_int array_len)));
              log_e = idx;
              msg = Idx_out_of_bounds (loc, array_len);
            }
          in
          let assert_val_fits_in_var =
            {
              Chp.Assert.cond = assert_fits_cond dst.d.dtype (Var (of_v dst));
              log_e = Var (of_v dst);
              msg = Value_doesnt_fit (loc, `Var);
            }
          in
          Seq
            [
              Seq asserts;
              Assert assert_idx_in_bounds;
              Par
                [
                  Send (loc, cmd_chan, LShift (idx, Const CInt.one));
                  ReadThenAssert
                    (loc, read_chan, of_v dst, assert_val_fits_in_var);
                ];
            ]
      | WriteUGMem (loc, mem, idx, value) ->
          let cmd_chan, write_chan, _, _, _ = chans_of_mem mem in
          let idx, asserts1 = of_e idx loc in
          let value, asserts2 = of_e value loc in
          let array_len = Array.length mem.d.init in
          let assert_idx_in_bounds =
            {
              Chp.Assert.cond = Expr.(Lt (idx, Const (CInt.of_int array_len)));
              log_e = idx;
              msg = Idx_out_of_bounds (loc, array_len);
            }
          in
          let assert_val_fits_in_cell =
            {
              Chp.Assert.cond = assert_fits_cond mem.d.dtype value;
              log_e = value;
              msg = Value_doesnt_fit (loc, `Mem_cell);
            }
          in
          Seq
            [
              Seq asserts1;
              Seq asserts2;
              Assert assert_idx_in_bounds;
              Assert assert_val_fits_in_cell;
              Par
                [
                  Send
                    ( loc,
                      cmd_chan,
                      BitOr (LShift (idx, Const CInt.one), Const CInt.one) );
                  Send (loc, write_chan, value);
                ];
            ]
      | WaitUntilReadReady _ | WaitUntilSendReady _ ->
          failwith
            "TODO - flat_program WaitUntilReadReady and WaitUntilSendReady are unimplemented"
    in
    let n = of_n n in
    (* Then add on all the initializers *)
    (* This map conversion makes the order deterministic *)
    let inits =
      Hashtbl.to_alist var_of_var
      |> Ir.Var.U.Map.of_alist_exn |> Map.to_alist
      |> List.map ~f:(fun (var, var_id) ->
             let init =
               Option.map var.d.init ~f:(fun init ->
                   Ir.DType.cint_of_value var.d.dtype init)
               |> Option.value ~default:CInt.zero
             in
             Chp.Stmt.Assign (Code_pos.dummy_loc, var_id, Const init))
    in
    Chp.Stmt.Seq [ Seq inits; n ]
  in

  let stmt = of_chp proc in

  let get_chans ~dir =
    let rec f n =
      match n with
      | Chp.Stmt.Nop | Assign _ | Log _ | Assert _ -> []
      | Seq ls | Par ls | SelectImm (_, _, ls) -> List.concat_map ls ~f
      | DoWhile (_, n, _) -> f n
      | Send (_, c, _) -> ( match dir with `Send -> [ c ] | `Read -> [])
      | ReadThenAssert (_, c, _, _) -> (
          match dir with `Send -> [] | `Read -> [ c ])
    in
    f stmt |> Chp.Chan.Set.of_list
  in

  let read_chans = get_chans ~dir:`Read in
  let write_chans = get_chans ~dir:`Send in

  let interproc_chan_of_chan chan =
    match
      Hashtbl.to_alist chan_of_chan
      |> List.find ~f:(fun (_, chan_) -> Chp.Chan.equal chan chan_)
    with
    | Some (ir_chan, _) -> interproc_chan_of_ir_chan ir_chan
    | None -> new_interproc_chan Code_pos.dummy_loc chan.bitwidth
  in

  let iports =
    Set.diff read_chans write_chans |> Map.of_key_set ~f:interproc_chan_of_chan
  in
  let oports =
    Set.diff write_chans read_chans |> Map.of_key_set ~f:interproc_chan_of_chan
  in

  let mems =
    mems_table |> Hashtbl.to_alist |> Act.Internal_rep.Mem.Map.of_alist_exn
  in
  let mems =
    Map.map mems
      ~f:(fun (cmd_chan, write_chan, read_chan, idx_bits, cell_bits) ->
        ( ( Map.find_exn oports cmd_chan,
            Map.find oports write_chan,
            Map.find iports read_chan ),
          (idx_bits, cell_bits) ))
  in
  let iports = Map.to_alist iports |> List.map ~f:(fun (a, b) -> (b, a)) in
  let oports = Map.to_alist oports |> List.map ~f:(fun (a, b) -> (b, a)) in
  ({ Chp.Proc.stmt; iports; oports; dflowable }, mems)

let of_program ir ~user_sendable_ports ~user_readable_ports =
  let module Ir = Act.Internal_rep in
  let next_interproc_chan_id = ref 0 in
  let interproc_chan_of_ir_chan_tbl = Ir.Chan.U.Table.create () in
  let new_interproc_chan creation_code_pos bitwidth =
    let id = !next_interproc_chan_id in
    let id = Interproc_chan.Id.of_int id in
    incr next_interproc_chan_id;
    { Interproc_chan.id; bitwidth; creation_code_pos }
  in
  let interproc_chan_of_ir_chan ir_chan =
    Hashtbl.find_or_add interproc_chan_of_ir_chan_tbl ir_chan
      ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout ir_chan.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        new_interproc_chan ir_chan.d.creation_code_pos bitwidth)
  in

  let program = Ir.Program.unwrap ir in
  let chp_procs, mem_maps =
    List.map program ~f:(fun process ->
        match process with
        | Dflow_iface_on_chp chp ->
            of_chp chp ~dflowable:true ~new_interproc_chan
              ~interproc_chan_of_ir_chan
        | Chp chp ->
            of_chp chp ~dflowable:false ~new_interproc_chan
              ~interproc_chan_of_ir_chan)
    |> List.unzip
  in
  let chp_procs =
    List.map chp_procs ~f:(fun proc -> { Process.k = Chp proc })
  in

  let mem_procs =
    List.concat_map mem_maps ~f:Map.to_alist
    |> Act.Internal_rep.Mem.Map.of_alist_multi |> Map.to_alist
    |> List.concat_map ~f:(fun (mem, chans) ->
           let (chans, bits), ctrl_procs =
             match chans with
             | [] -> failwith "unreachable"
             | [ (chans, bits) ] -> ((chans, bits), [])
             | chans ->
                 let chans, bits = List.unzip chans in
                 let bits = List.unzip bits in
                 let int_all_eq_val l =
                   List.all_equal l ~equal:Int.equal |> Option.value_exn
                 in
                 let idx_bits = int_all_eq_val (fst bits) in
                 let cell_bits = int_all_eq_val (snd bits) in
                 let cmd_chan =
                   new_interproc_chan Code_pos.dummy_loc (idx_bits + 1)
                 in
                 let write_chan =
                   new_interproc_chan Code_pos.dummy_loc cell_bits
                 in
                 let read_chan =
                   new_interproc_chan Code_pos.dummy_loc cell_bits
                 in
                 let ctrl_proc =
                   let _chans = chans in
                   failwith "TODO"
                 in
                 let chans = (cmd_chan, Some write_chan, Some read_chan) in
                 let bits = (idx_bits, cell_bits) in
                 ((chans, bits), [ ctrl_proc ])
           in
           let cmd_chan, write_chan, read_chan = chans in
           let idx_bits, cell_bits = bits in

           let init =
             Array.map mem.d.init ~f:(fun init_val ->
                 Ir.DType.cint_of_value mem.d.dtype init_val)
           in
           let mem_proc =
             {
               Mem_proc.init;
               idx_bits;
               cell_bits;
               cmd_chan;
               write_chan;
               read_chan = Option.value_exn read_chan;
             }
           in
           let mem_proc = { Process.k = Mem mem_proc } in
           mem_proc :: ctrl_procs)
  in

  let top_iports =
    List.map user_sendable_ports ~f:(fun chan ->
        let chan = Act.Internal_rep.Chan.unwrap_wu chan in
        let interproc_chan = interproc_chan_of_ir_chan chan in
        (interproc_chan, chan))
  in
  let top_oports =
    List.map user_readable_ports ~f:(fun chan ->
        let chan = Act.Internal_rep.Chan.unwrap_ru chan in
        let interproc_chan = interproc_chan_of_ir_chan chan in
        (interproc_chan, chan))
  in
  let processes = List.concat [ chp_procs; mem_procs ] in
  { Program.processes; top_iports; top_oports }

(* let check_n n ~user_sendable_ports ~user_readable_ports =
   (* assume n is a top-level statement. For now, we will just unilaterally impose
      dflow symatics. TODO require a chp node to weaken synmatics. *)

   (* First check there are no unsupported nodes *)
   let rec check_sup_nodes n =
     match n with
     | Ir.Chp.Par (_, ns) -> List.map ns ~f:check_sup_nodes |> Result.all_unit
     | Seq (_, ns) -> List.map ns ~f:check_sup_nodes |> Result.all_unit
     | Loop (_, n) -> check_sup_nodes n
     | SelectImm (_, branches, else_) ->
         let ns = List.map branches ~f:snd @ Option.to_list else_ in
         List.map ns ~f:check_sup_nodes |> Result.all_unit
     | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_sup_nodes seq
     | Assign (_, _, _) -> Ok ()
     | Send (_, _, _) -> Ok ()
     | Read (_, _, _) -> Ok ()
     | Nop -> Ok ()
     (* Factor these into a seperate process? *)
     | ReadUGMem (_, _, _, _) -> Ok ()
     | WriteUGMem (_, _, _, _) -> Ok ()
     (* These cant be supported in dflow *)
     | WaitUntilReadReady (_, _) ->
         Error "dflow does not support probes but has WaitUntilReadReady"
     | WaitUntilSendReady (_, _) ->
         Error "dflow does not support probes but has WaitUntilSendReady"
     (* Should these be supported, or should they just be dropped? *)
     | Log _ | Log1 _ | Assert _ -> Ok ()
   in
   let%bind.Result () = check_sup_nodes n in

   let dummy_chan_of_mem_table = Ir.Mem.Table.create () in
   let dummy_chan_of_mem mem =
     Hashtbl.find_or_add dummy_chan_of_mem_table mem ~default:(fun () ->
         Act.Chan.W.create (CInt.dtype ~bits:1) |> Ir.Chan.unwrap_w)
   in

   (* check that par branches dont both use the same side of the same channel *)
   let rec chans n ~r ~w =
     let f n = chans n ~r ~w in
     match n with
     | Ir.Chp.Par (_, ns) -> List.concat_map ns ~f
     | Seq (_, ns) -> List.concat_map ns ~f
     | Loop (_, n) -> f n
     | SelectImm (_, branches, else_) -> (
         List.concat_map branches ~f:(fun (_, n) -> f n)
         @ match else_ with Some else_ -> f else_ | None -> [])
     | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> f seq
     | Assign (_, _, _) -> []
     | Send (_, chan, _) -> if w then [ chan ] else []
     | Read (_, chan, _) -> if r then [ chan ] else []
     | Nop -> []
     | ReadUGMem (_, mem, _, _) -> [ dummy_chan_of_mem mem ]
     | WriteUGMem (_, mem, _, _) -> [ dummy_chan_of_mem mem ]
     | Log _ | Log1 _ | Assert _ -> []
     | WaitUntilReadReady (_, _) | WaitUntilSendReady (_, _) ->
         failwith "unreachable: handled above"
   in
   let r_chans n = chans n ~r:true ~w:false |> Ir.Chan.U.Set.of_list in
   let w_chans n = chans n ~r:false ~w:true |> Ir.Chan.U.Set.of_list in

   let subsets_2 l =
     List.mapi l ~f:(fun i x -> (i, x))
     |> List.concat_map ~f:(fun (i, x) ->
            List.drop l (i + 1) |> List.map ~f:(fun y -> (x, y)))
   in
   let find_conflicting_pair ns ~f =
     let ns = List.map ns ~f:(fun n -> (n, f n)) in
     subsets_2 ns
     |> List.filter_map ~f:(fun ((n1, l1), (n2, l2)) ->
            match Set.inter l1 l2 |> Set.to_list with
            | [] -> None
            | x :: _ -> Some (n1, n2, x))
     |> List.hd
   in
   let rec check_par_nodes n =
     match n with
     | Ir.Chp.Par (_, ns) -> (
         let%bind.Result () =
           List.map ns ~f:check_par_nodes |> Result.all_unit
         in
         let%bind.Result () =
           match find_conflicting_pair ns ~f:r_chans with
           | None -> Ok ()
           | Some (_, _, _) ->
               Error
                 "Two branches of par block read the same channel. Dataflow \
                  converter does not suppor this"
         in
         match find_conflicting_pair ns ~f:w_chans with
         | None -> Ok ()
         | Some (_, _, _) ->
             Error
               "Two branches of par block write the same channel. Dataflow \
                converter does not suppor this.")
     | Seq (_, ns) -> List.map ns ~f:check_par_nodes |> Result.all_unit
     | Loop (_, n) -> check_par_nodes n
     | SelectImm (_, branches, else_) ->
         let ns = List.map branches ~f:snd @ Option.to_list else_ in
         List.map ns ~f:check_par_nodes |> Result.all_unit
     | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_par_nodes seq
     | Assign (_, _, _) -> Ok ()
     | Send (_, _, _) -> Ok ()
     | Read (_, _, _) -> Ok ()
     | Nop -> Ok ()
     (* Factor these into a seperate process? *)
     | ReadUGMem (_, _, _, _) -> Ok ()
     | WriteUGMem (_, _, _, _) -> Ok ()
     (* Should these be supported, or should they just be dropped? *)
     | Log _ | Log1 _ | Assert _ -> Ok ()
     (* These cant be supported in dflow *)
     | WaitUntilReadReady (_, _) | WaitUntilSendReady (_, _) ->
         failwith "unreachable: handled above"
   in
   (* TODO also check variables not used same side of par node *)
   let%bind.Result () = check_par_nodes n in

   (* Then check that each io channel is not also read/written in the program. *)
   let%bind.Result () =
     match Set.inter user_readable_ports (r_chans n) |> Set.to_list with
     | [] -> Ok ()
     | _ :: _ ->
         Error
           "Channel read in prgram but listed as user readable.. Dataflow \
            converter does not suppor this."
   in
   match Set.inter user_sendable_ports (w_chans n) |> Set.to_list with
   | [] -> Ok ()
   | _ :: _ ->
       Error
         "Channel written in prgram but listed as user sendable. Dataflow \
          converter does not suppor this." *)
