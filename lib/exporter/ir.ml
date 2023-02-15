open! Core
open! Act
module Ir = Internal_rep

module Code_pos = struct
  include Code_pos

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Var = struct
  module Id = Int

  module T = struct
    type t = { id : Id.t; bitwidth : int; creation_code_pos : Code_pos.t }
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
    type t = {
      id : Id.t;
      bitwidth : int;
      creation_code_pos : Code_pos.t;
          (* wait_readable_code_pos : Code_pos.t option; *)
          (* wait_sendable_code_pos : Code_pos.t option; *)
    }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Expr = struct
  type 'v t =
    | Var of 'v
    | Const of CInt.t
    | Add of 'v t * 'v t
    | Sub_no_wrap of 'v t * 'v t
    | Mul of 'v t * 'v t
    | Div of 'v t * 'v t
    | Mod of 'v t * 'v t
    | Eq of 'v t * 'v t
    | Ne of 'v t * 'v t
    | Gt of 'v t * 'v t
    | Ge of 'v t * 'v t
    | Lt of 'v t * 'v t
    | Le of 'v t * 'v t
    | BitXor of 'v t * 'v t
    | BitOr of 'v t * 'v t
    | BitAnd of 'v t * 'v t
    | LShift of 'v t * 'v t
    | RShift of 'v t * 'v t
    | Clip of 'v t * int
  [@@deriving sexp]

  let map_var_nodes e ~f =
    let rec h e =
      match e with
      | Var v -> (f v)
      | Const c -> Const c
      | Add (a, b) -> Add (h a, h b)
      | Sub_no_wrap (a, b) -> Add (h a, h b)
      | Mul (a, b) -> Mul (h a, h b)
      | Div (a, b) -> Div (h a, h b)
      | Mod (a, b) -> Mod (h a, h b)
      | Eq (a, b) -> Eq (h a, h b)
      | Ne (a, b) -> Ne (h a, h b)
      | Gt (a, b) -> Gt (h a, h b)
      | Ge (a, b) -> Ge (h a, h b)
      | Lt (a, b) -> Lt (h a, h b)
      | Le (a, b) -> Le (h a, h b)
      | BitXor (a, b) -> BitXor (h a, h b)
      | BitOr (a, b) -> BitOr (h a, h b)
      | BitAnd (a, b) -> BitAnd (h a, h b)
      | LShift (a, b) -> LShift (h a, h b)
      | RShift (a, b) -> RShift (h a, h b)
      | Clip (a, bits) -> Clip (h a, bits)
    in
    h e

  let map_vars e ~f = map_var_nodes e ~f:(fun v -> Var (f v))

  let var_ids e =
    let rec f e =
      match e with
      | Var v -> [ v ]
      | Const _ -> []
      | Add (a, b) -> f a @ f b
      | Sub_no_wrap (a, b) -> f a @ f b
      | Mul (a, b) -> f a @ f b
      | Div (a, b) -> f a @ f b
      | Mod (a, b) -> f a @ f b
      | Eq (a, b) -> f a @ f b
      | Ne (a, b) -> f a @ f b
      | Gt (a, b) -> f a @ f b
      | Ge (a, b) -> f a @ f b
      | Lt (a, b) -> f a @ f b
      | Le (a, b) -> f a @ f b
      | BitXor (a, b) -> f a @ f b
      | BitOr (a, b) -> f a @ f b
      | BitAnd (a, b) -> f a @ f b
      | LShift (a, b) -> f a @ f b
      | RShift (a, b) -> f a @ f b
      | Clip (a, _) -> f a
    in
    f e

  let bitwidth e ~bits_of_var =
    let rec h e =
      match e with
      | Var v -> bits_of_var v
      | Const c -> CInt.bitwidth c
      | Add (a, b) -> 1 + Int.max (h a) (h b)
      | Sub_no_wrap (a, _) -> h a
      | Mul (a, b) -> h a + h b
      | Div (a, _) -> h a
      | Mod (a, b) -> Int.min (h a) (h b)
      | Eq (_, _) -> 1
      | Ne (_, _) -> 1
      | Gt (_, _) -> 1
      | Ge (_, _) -> 1
      | Lt (_, _) -> 1
      | Le (_, _) -> 1
      | BitXor (a, b) -> Int.max (h a) (h b)
      | BitOr (a, b) -> Int.max (h a) (h b)
      | BitAnd (a, b) -> Int.min (h a) (h b)
      | LShift (a, b) -> h a + Int.pow 2 (h b) - 1
      | RShift (a, _) -> h a
      | Clip (a, bits) -> Int.min (h a) bits
    in
    h e
end

module Assert = struct
  type t = {
    cond : Var.t Expr.t;
    log_e : Var.t Expr.t;
    log_fn : CInt.t -> string;
  }
  [@@deriving sexp_of]
end

module Chp_stmt = struct
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

module Chp_proc = struct
  type t = Chp_stmt.t [@@deriving sexp_of]
end

module Mem_proc = struct
  type t = {
    init : CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Chan.t;
    read_chan : Chan.t;
    write_chan : Chan.t;
  }
  [@@deriving sexp_of]
end

module Proc = struct
  module K = struct
    type t =
      | Chp of Chp_proc.t
      | Mem of Mem_proc.t
      | INode of Chan.t * Act.Internal_rep.Chan.U.t
      | ONode of Chan.t * Act.Internal_rep.Chan.U.t
    [@@deriving sexp_of]
  end

  type t = { k : K.t; in_chans : Chan.Set.t; out_chans : Chan.Set.t }
end

let of_ir ir ~user_sendable_ports ~user_readable_ports =
  let n = Ir.Chp.unwrap ir in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in

  (* let%map.Result () = check_n n ~user_sendable_ports ~user_readable_ports in *)
  let next_v_id = ref 0 in
  let var_of_var = Ir.Var.U.Table.create () in
  let new_var creation_code_pos bitwidth =
    let id = !next_v_id in
    incr next_v_id;
    { Var.id; bitwidth; creation_code_pos }
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
    { Chan.id; bitwidth; creation_code_pos }
  in
  let of_c c =
    Hashtbl.find_or_add chan_of_chan c ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout c.d.dtype with Bits_fixed bitwidth -> bitwidth
        in

        new_chan c.d.creation_code_pos bitwidth)
  in

  let of_e e loc =
    let asserts = Queue.create () in
    let rec f e =
      match e with
      | Ir.Expr.K.Add (a, b) -> Expr.Add (f a, f b)
      | Sub_no_wrap (a, b) ->
          let a, b = (f a, f b) in
          let width_b =
              Expr.bitwidth b ~bits_of_var:Var.bitwidth 


          in
          let a_concat_b =
            Expr.BitOr (LShift (a, Const (CInt.of_int width_b)), b)
          in
          let msg i =
            let b_mask = CInt.(pow (of_int 2) (of_int width_b) - of_int 1) in
            let b = CInt.bit_and i b_mask in
            let a = CInt.shift_right_logical i (CInt.of_int width_b) in
            [%string
              "Subtraction underflowed when computing %{a#CInt} - %{b#CInt}"]
          in
          Queue.enqueue asserts (Expr.Ge (a, b), a_concat_b, msg);
          Sub_no_wrap (a, b)
      | Sub_wrap (a, b, bits) ->
          let p2bits =
            Expr.Const (CInt.shift_left CInt.one (CInt.of_int bits))
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
      | Var v -> Var (of_v v)
      | Clip (e, bits) -> Clip (f e, bits)
      | Const c -> Const c
      | With_assert_log (a, v, l, ff) ->
          let a = f a in
          let v = f v in
          let l = f l in
          let msg ll = [%string "Assertion failure: %{ff ll}"] in
          Queue.enqueue asserts (a, l, msg);
          v
    in
    let e = f e.Ir.Expr.k in
    let asserts =
      Queue.to_list asserts
      |> List.map ~f:(fun (cond, log_e, ff) ->
             let log_fn vl =
               [%string
                 "Failed to evaluate expression on line %{loc#Code_pos}: %{ff \
                  vl}"]
             in
             let assert_ = { Assert.cond; log_e; log_fn } in
             Chp_stmt.Assert assert_)
    in
    (e, asserts)
  in

  (* This IR should encode as many runtime asserts in the tree as possible. The idea is to
      have a simple uniform representation of the program that works either for simulation
     or code generation, and so it must maintain lines of code for now. *)

  (* We pull each mem out into seperate process *)
  let mems_table = Ir.Mem.Table.create () in
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


  let assert_fits_cond _dtype e =
    let dummy_var = Act.Var.create (CInt.dtype ~bits:(Expr.bitwidth e ~bits_of_var:Var.bitwidth)) in
    let dummy_expr = failwith "TODO" in 
    (* DType.assert_fits_expr dtype ~dummy_var |> of_e in *)
    let dummy_var = of_v (Ir.Var.unwrap dummy_var).u in
    Expr.map_var_nodes dummy_expr ~f:(fun v -> if Var.equal v dummy_var then e else Var v)
  in

  let rec of_n n =
    match n with
    | Ir.Chp.Par (_, ns) -> Chp_stmt.Par (List.map ns ~f:of_n)
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
        let log_fn _ = [%string "Assertion failure at %{loc#Code_pos}"] in
        let assert_ = { Assert.cond = e; log_e = Const CInt.zero; log_fn } in
        Seq [ Seq asserts; Assert assert_ ]
    | Loop (loc, n) -> DoWhile (loc, of_n n, Expr.Const CInt.one)
    | DoWhile (loc, seq, expr) ->
        let expr, asserts = of_e expr loc in
        DoWhile (loc, Seq [ of_n seq; Seq asserts ], expr)
    | Assign (loc, id, expr) ->
        let expr, asserts = of_e expr loc in
        let assert_val_fits_in_var =
          {
            Assert.cond = assert_fits_cond id.d.dtype expr;
            log_e = expr;
            log_fn = (fun _ -> failwith "TODO");
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
            Assert.cond = assert_fits_cond chan.d.dtype expr;
            log_e = expr;
            log_fn = (fun _ -> failwith "TODO");
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
            Assert.cond = assert_fits_cond var.d.dtype (Var (of_v var));
            log_e =  (Var (of_v var));
            log_fn = (fun _ -> failwith "TODO");
          }
        in
        ReadThenAssert (loc, of_c chan, of_v var, assert_val_fits_in_var)
    | SelectImm (loc, branches, else_) ->
        let guards = List.map branches ~f:(fun (guard, _) -> of_e guard loc) in
        let else_guard =
          match else_ with
          | Some _ ->
              let any_guard_true =
                List.map guards ~f:fst |> List.reduce  ~f:(fun a b -> Expr.BitOr (a, b))
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
        Seq (List.concat guard_asserts @ [ SelectImm (loc, guard_expr, stmts) ])
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
            Assert.cond = Expr.(Lt (idx, Const (CInt.of_int array_len)));
            log_e = idx;
            log_fn =
              (fun idx ->
                 [%string
                    "idx %{idx#CInt} is to big for array of length \
                     %{array_len#Int}."]);
          }
        in
        let assert_val_fits_in_var =
          {
            Assert.cond = assert_fits_cond dst.d.dtype (Var (of_v dst));
            log_e = (Var (of_v dst));
            log_fn = (fun _ -> failwith "TODO");
          }
        in
        Seq
          [
            Seq asserts;
            Assert assert_idx_in_bounds;
            Par
              [
                Send (loc, cmd_chan, LShift (idx, Const CInt.one));
                ReadThenAssert (loc, read_chan, of_v dst, assert_val_fits_in_var);
              ];
          ]
    | WriteUGMem (loc, mem, idx, value) ->
        let cmd_chan, write_chan, _, _, _ = chans_of_mem mem in
        let idx, asserts1 = of_e idx loc in
        let value, asserts2 = of_e value loc in
        let array_len = Array.length mem.d.init in
        let assert_idx_in_bounds =
          {
            Assert.cond = Expr.(Lt (idx, Const (CInt.of_int array_len)));
            log_e = idx;
            log_fn =
              (fun idx ->
                [%string
                    "idx %{idx#CInt} is to big for array of length \
                     %{array_len#Int}."]);
          }
        in
        let assert_val_fits_in_cell =
          {
            Assert.cond = assert_fits_cond mem.d.dtype value;
            log_e = value;
            log_fn = (fun _ -> failwith "TODO");
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
        failwith "TODO - exporter bug. Should have checked this in is_dflowable"
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
           Chp_stmt.Assign (Code_pos.dummy_loc, var_id, Const init))
  in
  let n = Chp_stmt.Seq [ Seq inits; n ] in
  let chp_proc =
    { Proc.k = Chp n; in_chans = failwith "TODO"; out_chans = failwith "TODO" }
  in
  let mem_procs =
    Hashtbl.to_alist mems_table
    |> Ir.Mem.Map.of_alist_exn |> Map.to_alist
    |> List.map
         ~f:(fun (mem, (cmd_chan, write_chan, read_chan, idx_bits, cell_bits))
            ->
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
               read_chan;
             }
           in
           {
             Proc.k = Mem mem_proc;
             in_chans = failwith "TODO";
             out_chans = failwith "TODO";
           })
  in
  let inodes =
    Set.to_list user_sendable_ports
    |> List.map ~f:(fun chan ->
           let my_chan = Hashtbl.find_exn chan_of_chan chan in
           {
             Proc.k = INode (my_chan, chan);
             in_chans = Chan.Set.empty;
             out_chans = Chan.Set.singleton my_chan;
           })
  in
  let onodes =
    Set.to_list user_readable_ports
    |> List.map ~f:(fun chan ->
           let my_chan = Hashtbl.find_exn chan_of_chan chan in
           {
             Proc.k = ONode (my_chan, chan);
             in_chans = Chan.Set.singleton my_chan;
             out_chans = Chan.Set.empty;
           })
  in
  let procs = List.concat [ [ chp_proc ]; mem_procs; inodes; onodes ] in
  procs

let of_ir ir ~user_sendable_ports ~user_readable_ports =
  of_ir ir ~user_sendable_ports ~user_readable_ports

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
