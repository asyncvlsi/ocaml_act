open! Core
open Utils

module Var = struct
  module Id = Int

  module T = struct
    type t = { id : Id.t; bitwidth : int }
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
    type t = { id : Id.t; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
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
    | Nop
    | Assert of Var.t Ir.Expr.t
    | Assign of Var.t * Var.t Ir.Expr.t
    | Seq of t list
    | Par of t list
    (* assert happens immediatly after read before any other code runs *)
    | ReadThenAssert of Chan.t * Var.t * Var.t Ir.Expr.t
    | Send of Chan.t * Var.t Ir.Expr.t
    | DoWhile of t * Var.t Ir.Expr.t
      (* This expr is a one-hot vector with List.length branches bits indexing
         into the list of branches *)
    | SelectImm of Var.t Ir.Expr.t list * t list
    | Nondeterm_select of (Probe.t * t) list
  [@@deriving sexp_of]

  let rec flatten stmt =
    match stmt with
    | Par ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n -> match n with Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n with Par ns -> ns | _ -> [ n ])
        in
        match ns with [] -> Nop | [ n ] -> n | ls -> Par ls)
    | Seq ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n -> match n with Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n with Seq ns -> ns | _ -> [ n ])
        in
        match ns with [] -> Nop | [ n ] -> n | ls -> Seq ls)
    | SelectImm (guards, branches) ->
        let branches = List.map branches ~f:flatten in
        if
          List.exists branches ~f:(fun branch ->
              match branch with Nop -> false | _ -> true)
        then SelectImm (guards, branches)
        else Nop
    | Nondeterm_select branches ->
        Nondeterm_select
          (List.map branches ~f:(fun (probe, stmt) -> (probe, flatten stmt)))
    | DoWhile (seq, expr) -> DoWhile (flatten seq, expr)
    | Assign (id, expr) -> Assign (id, expr)
    | Send (chan, expr) -> Send (chan, expr)
    | ReadThenAssert (chan, var, assert_) -> ReadThenAssert (chan, var, assert_)
    | Assert assert_ -> Assert assert_
    | Nop -> Nop
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

let of_chp (proc : Ir.Chp.t) ~new_interproc_chan ~interproc_chan_of_ir_chan
    ~dflowable =
  let next_v_id = ref 0 in
  let var_of_var = Ir.Var.Table.create () in
  let new_var bitwidth =
    let id = !next_v_id in
    incr next_v_id;
    { Var.id; bitwidth }
  in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () -> new_var v.bitwidth)
  in

  let next_c_id = ref 0 in
  let chan_of_chan = Ir.Chan.Table.create () in
  let new_chan bitwidth =
    let id = !next_c_id in
    incr next_c_id;
    { Chan.id; bitwidth }
  in
  let of_c c =
    Hashtbl.find_or_add chan_of_chan c ~default:(fun () -> new_chan c.bitwidth)
  in

  let of_e0 e ~of_assert ~of_var =
    let rec f e =
      match e with
      | Ir.Expr.Add (a, b) -> Ir.Expr.Add (f a, f b)
      | Sub_no_wrap (a, b) ->
          let a, b = (f a, f b) in
          of_assert (Ir.Expr.Ge (a, b));
          Sub_no_wrap (a, b)
      | Sub_wrap (a, b, bits) ->
          let p2bits =
            Ir.Expr.Const (CInt.left_shift CInt.one ~amt:(CInt.of_int bits))
          in
          let a = Ir.Expr.BitOr (Clip (f a, bits), p2bits) in
          let b = Ir.Expr.Clip (f b, bits) in
          Ir.Expr.Clip (Sub_no_wrap (a, b), bits)
      | Mul (a, b) -> Mul (f a, f b)
      | Div (a, b) -> Div (f a, f b)
      | Mod (a, b) -> Mod (f a, f b)
      | LShift (a, b) -> LShift (f a, f b)
      | RShift (a, b) -> RShift (f a, f b)
      | BitAnd (a, b) -> BitAnd (f a, f b)
      | BitOr (a, b) -> BitOr (f a, f b)
      | BitXor (a, b) -> BitXor (f a, f b)
      | Eq0 a -> Eq0 (f a)
      | Eq (a, b) -> Eq (f a, f b)
      | Ne (a, b) -> Ne (f a, f b)
      | Lt (a, b) -> Lt (f a, f b)
      | Le (a, b) -> Le (f a, f b)
      | Gt (a, b) -> Gt (f a, f b)
      | Ge (a, b) -> Ge (f a, f b)
      | Var v -> of_var v
      | Clip (e, bits) -> Clip (f e, bits)
      | Const c -> Const c
      | Concat l -> Concat (List.map l ~f:(fun (e, bits) -> (f e, bits)))
      | Log2OneHot a -> Log2OneHot (f a)
    in

    f e
  in

  let of_e e =
    let asserts = Queue.create () in
    let e =
      of_e0 e
        ~of_var:(fun v -> Var (of_v v))
        ~of_assert:(fun cond -> Queue.enqueue asserts (Stmt.Assert cond))
    in
    (e, Queue.to_list asserts)
  in

  (* This IR should encode as many runtime asserts in the tree as possible. The
     idea is to have a simple uniform representation of the program that works
     either for simulation or code generation, and so it must maintain lines of
     code for now. *)

  (* We pull each mem out into seperate process *)
  let mems_table = Ir.Mem.Table.create () in
  let chans_of_mem mem =
    Hashtbl.find_or_add mems_table mem ~default:(fun () ->
        let idx_bits = mem.init |> Array.length |> Int.ceil_log2 in
        let cell_bits = mem.cell_bitwidth in
        (* TODO carry better error messages with these channels *)
        ( new_chan (1 + idx_bits),
          new_chan cell_bits,
          new_chan cell_bits,
          idx_bits,
          cell_bits ))
  in

  (* let assert_fits_cond dtype e = *)
  (* TODO for now ignoring asserts here *)
  (* let a = Ir_dtype.of_cint_assert_expr_fn dtype in *)
  (* of_e0 a ~of_var:(fun () -> e) ~of_assert:(fun _ -> ()) *)
  (* in *)
  let of_chp n =
    let rec of_n n =
      match n with
      | Ir.Chp.Par { m = _; ns } -> Stmt.Par (List.map ns ~f:of_n)
      | Seq { m = _; ns } -> Seq (List.map ns ~f:of_n)
      | Nop _ -> Nop
      | Log1 _ -> Nop
      | Assert { m = _; expr; log_e = _; msg_fn = _ } ->
          let expr, asserts = of_e expr in
          Seq [ Seq asserts; Assert expr ]
      (* | Loop (_, n) -> DoWhile (of_n n, Ir.Expr.Const CInt.one) *)
      | DoWhile { m = _; n; g } ->
          let g, asserts = of_e g in
          DoWhile (Seq [ of_n n; Seq asserts ], g)
      | Assign { m = _; var; expr } ->
          let expr, asserts = of_e expr in
          Seq
            [
              Seq asserts;
              (* Assert (assert_fits_cond var.d.dtype expr); *)
              Assign (of_v var, expr);
            ]
      | Send { m = _; chan; expr } ->
          let expr, asserts = of_e expr in
          Seq
            [
              Seq asserts;
              (* Assert (assert_fits_cond chan.d.dtype expr); *)
              Send (of_c chan, expr);
            ]
      | Read { m = _; chan; var } ->
          ReadThenAssert
            ( of_c chan,
              of_v var,
              Ir.Expr.Const CInt.one
              (* assert_fits_cond var.d.dtype (Var (of_v var)) *) )
      | SelectImm { m = _; branches; else_ } ->
          let guards = List.map branches ~f:(fun (guard, _) -> of_e guard) in
          let else_guard =
            match else_ with
            | Some _ ->
                let any_guard_true =
                  List.map guards ~f:fst
                  |> List.reduce ~f:(fun a b -> Ir.Expr.BitOr (a, b))
                  |> Option.value ~default:(Ir.Expr.Const CInt.zero)
                in
                [ (Ir.Expr.Eq0 any_guard_true, []) ]
            | None -> []
          in
          let guards, guard_asserts = guards @ else_guard |> List.unzip in
          let stmts =
            List.map branches ~f:snd @ Option.to_list else_ |> List.map ~f:of_n
          in
          (* TODO do this better *)
          Seq (List.concat guard_asserts @ [ SelectImm (guards, stmts) ])
      | WhileLoop { m = _; g; n } ->
          let g, asserts = of_e g in
          Seq
            [
              Seq asserts;
              SelectImm
                ([ Eq0 g; g ], [ Nop; DoWhile (Seq [ of_n n; Seq asserts ], g) ]);
            ]
      | ReadMem { m = _; mem; idx; var = dst } ->
          let idx, asserts = of_e idx in
          let cmd_chan, _, read_chan, _, _ = chans_of_mem mem in
          let array_len = Array.length mem.init in
          Seq
            [
              Seq asserts;
              Assert (Lt (idx, Const (CInt.of_int array_len)));
              Par
                [
                  Send (cmd_chan, LShift (idx, Const CInt.one));
                  ReadThenAssert
                    ( read_chan,
                      of_v dst,
                      Ir.Expr.Const CInt.one
                      (* assert_fits_cond dst.d.dtype (Var (of_v dst)) *) );
                ];
            ]
      | WriteMem { m = _; mem; idx; expr = value } ->
          let cmd_chan, write_chan, _, _, _ = chans_of_mem mem in
          let idx, asserts1 = of_e idx in
          let value, asserts2 = of_e value in
          let array_len = Array.length mem.init in
          Seq
            [
              Seq asserts1;
              Seq asserts2;
              Assert (Lt (idx, Const (CInt.of_int array_len)));
              (* Assert (assert_fits_cond mem.d.dtype value); *)
              Par
                [
                  Send
                    ( cmd_chan,
                      Concat
                        [ (Const CInt.one, 1); (idx, Int.ceil_log2 array_len) ]
                    );
                  Send (write_chan, value);
                ];
            ]
      | Nondeterm_select { m = _; branches } ->
          let branches =
            List.map branches ~f:(fun (probe, stmt) ->
                let probe =
                  match probe with
                  | Read chan -> Probe.Read (of_c chan)
                  | Send chan -> Send (of_c chan)
                in
                (probe, of_n stmt))
          in
          Nondeterm_select branches
    in
    let n = of_n n in
    (* Then add on all the initializers *)
    (* This map conversion makes the order deterministic *)
    let inits =
      Hashtbl.to_alist var_of_var
      |> Ir.Var.Map.of_alist_exn |> Map.to_alist
      |> List.map ~f:(fun (var, var_id) ->
             let init = Option.value var.init ~default:CInt.zero in
             Stmt.Assign (var_id, Const init))
    in
    Stmt.Seq [ Seq inits; n ]
  in

  let stmt = of_chp proc in

  let get_chans ~dir =
    let rec f n =
      match n with
      | Stmt.Nop | Assign _ | Assert _ -> []
      | Seq ls | Par ls | SelectImm (_, ls) -> List.concat_map ls ~f
      | Nondeterm_select ls -> List.concat_map ls ~f:(fun (_, stmt) -> f stmt)
      | DoWhile (n, _) -> f n
      | Send (c, _) -> ( match dir with `Send -> [ c ] | `Read -> [])
      | ReadThenAssert (c, _, _) -> (
          match dir with `Send -> [] | `Read -> [ c ])
    in
    f stmt |> Chan.Set.of_list
  in

  let read_chans = get_chans ~dir:`Read in
  let write_chans = get_chans ~dir:`Send in

  let interproc_chan_of_chan chan =
    match
      Hashtbl.to_alist chan_of_chan
      |> List.find ~f:(fun (_, chan_) -> Chan.equal chan chan_)
    with
    | Some (ir_chan, _) -> interproc_chan_of_ir_chan ir_chan
    | None -> new_interproc_chan chan.bitwidth
  in

  let iports =
    Set.diff read_chans write_chans |> Map.of_key_set ~f:interproc_chan_of_chan
  in
  let oports =
    Set.diff write_chans read_chans |> Map.of_key_set ~f:interproc_chan_of_chan
  in

  let mems = mems_table |> Hashtbl.to_alist |> Ir.Mem.Map.of_alist_exn in
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
  ({ Proc.stmt = Stmt.flatten stmt; iports; oports; dflowable }, mems)

(* let check_n n ~user_sendable_ports ~user_readable_ports = (* assume n is a
   top-level statement. For now, we will just unilaterally impose dflow
   symatics. TODO require a chp node to weaken synmatics. *)

   (* First check there are no unsupported nodes *) let rec check_sup_nodes n =
   match n with | Ir.Chp.Par (_, ns) -> List.map ns ~f:check_sup_nodes |>
   Result.all_unit | Seq (_, ns) -> List.map ns ~f:check_sup_nodes |>
   Result.all_unit | Loop (_, n) -> check_sup_nodes n | SelectImm (_, branches,
   else_) -> let ns = List.map branches ~f:snd @ Option.to_list else_ in
   List.map ns ~f:check_sup_nodes |> Result.all_unit | WhileLoop (_, _, seq) |
   DoWhile (_, seq, _) -> check_sup_nodes seq | Assign (_, _, _) -> Ok () | Send
   (_, _, _) -> Ok () | Read (_, _, _) -> Ok () | Nop -> Ok () (* Factor these
   into a seperate process? *) | ReadUGMem (_, _, _, _) -> Ok () | WriteUGMem
   (_, _, _, _) -> Ok () (* These cant be supported in dflow *) |
   WaitUntilReadReady (_, _) -> Error "dflow does not support probes but has
   WaitUntilReadReady" | WaitUntilSendReady (_, _) -> Error "dflow does not
   support probes but has WaitUntilSendReady" (* Should these be supported, or
   should they just be dropped? *) | Log _ | Log1 _ | Assert _ -> Ok () in
   let%bind.Result () = check_sup_nodes n in

   let dummy_chan_of_mem_table = Ir.Mem.Table.create () in let dummy_chan_of_mem
   mem = Hashtbl.find_or_add dummy_chan_of_mem_table mem ~default:(fun () ->
   Chan.W.create (CInt.dtype ~bits:1) |> Ir.Chan.unwrap_w) in

   (* check that par branches dont both use the same side of the same channel *)
   let rec chans n ~r ~w = let f n = chans n ~r ~w in match n with | Ir.Chp.Par
   (_, ns) -> List.concat_map ns ~f | Seq (_, ns) -> List.concat_map ns ~f |
   Loop (_, n) -> f n | SelectImm (_, branches, else_) -> ( List.concat_map
   branches ~f:(fun (_, n) -> f n) @ match else_ with Some else_ -> f else_ |
   None -> []) | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> f seq | Assign
   (_, _, _) -> [] | Send (_, chan, _) -> if w then [ chan ] else [] | Read (_,
   chan, _) -> if r then [ chan ] else [] | Nop -> [] | ReadUGMem (_, mem, _, _)
   -> [ dummy_chan_of_mem mem ] | WriteUGMem (_, mem, _, _) -> [
   dummy_chan_of_mem mem ] | Log _ | Log1 _ | Assert _ -> [] |
   WaitUntilReadReady (_, _) | WaitUntilSendReady (_, _) -> failwith
   "unreachable: handled above" in let r_chans n = chans n ~r:true ~w:false |>
   Ir.Chan.Set.of_list in let w_chans n = chans n ~r:false ~w:true |>
   Ir.Chan.Set.of_list in

   let subsets_2 l = List.mapi l ~f:(fun i x -> (i, x)) |> List.concat_map
   ~f:(fun (i, x) -> List.drop l (i + 1) |> List.map ~f:(fun y -> (x, y))) in
   let find_conflicting_pair ns ~f = let ns = List.map ns ~f:(fun n -> (n, f n))
   in subsets_2 ns |> List.filter_map ~f:(fun ((n1, l1), (n2, l2)) -> match
   Set.inter l1 l2 |> Set.to_list with | [] -> None | x :: _ -> Some (n1, n2,
   x)) |> List.hd in let rec check_par_nodes n = match n with | Ir.Chp.Par (_,
   ns) -> ( let%bind.Result () = List.map ns ~f:check_par_nodes |>
   Result.all_unit in let%bind.Result () = match find_conflicting_pair ns
   ~f:r_chans with | None -> Ok () | Some (_, _, _) -> Error "Two branches of
   par block read the same channel. Dataflow \ converter does not suppor this"
   in match find_conflicting_pair ns ~f:w_chans with | None -> Ok () | Some (_,
   _, _) -> Error "Two branches of par block write the same channel. Dataflow \
   converter does not suppor this.") | Seq (_, ns) -> List.map ns
   ~f:check_par_nodes |> Result.all_unit | Loop (_, n) -> check_par_nodes n |
   SelectImm (_, branches, else_) -> let ns = List.map branches ~f:snd @
   Option.to_list else_ in List.map ns ~f:check_par_nodes |> Result.all_unit |
   WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_par_nodes seq | Assign
   (_, _, _) -> Ok () | Send (_, _, _) -> Ok () | Read (_, _, _) -> Ok () | Nop
   -> Ok () (* Factor these into a seperate process? *) | ReadUGMem (_, _, _, _)
   -> Ok () | WriteUGMem (_, _, _, _) -> Ok () (* Should these be supported, or
   should they just be dropped? *) | Log _ | Log1 _ | Assert _ -> Ok () (* These
   cant be supported in dflow *) | WaitUntilReadReady (_, _) |
   WaitUntilSendReady (_, _) -> failwith "unreachable: handled above" in (* TODO
   also check variables not used same side of par node *) let%bind.Result () =
   check_par_nodes n in

   (* Then check that each io channel is not also read/written in the program.
   *) let%bind.Result () = match Set.inter user_readable_ports (r_chans n) |>
   Set.to_list with | [] -> Ok () | _ :: _ -> Error "Channel read in prgram but
   listed as user readable.. Dataflow \ converter does not suppor this." in
   match Set.inter user_sendable_ports (w_chans n) |> Set.to_list with | [] ->
   Ok () | _ :: _ -> Error "Channel written in prgram but listed as user
   sendable. Dataflow \ converter does not suppor this." *)
