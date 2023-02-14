open! Core
open! Act
module Ir = Internal_rep

module Var = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Chan = struct
  module T = struct
    type t = { id : int; bitwidth : int }
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

  let map_vars e ~f =
    let rec h e =
      match e with
      | Var v -> Var (f v)
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

module Chp_stmt = struct
  type t =
    | Nop
    | Assign of Var.t * Var.t Expr.t
    | Seq of t list
    | Par of t list
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t Expr.t
    | DoWhile of t * Var.t Expr.t
      (* This expr is a one-hot vector with List.length branches bits
          indexing into the list of branches *)
    | SelectImm of Var.t Expr.t * t list
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

let check_n n ~user_sendable_ports ~user_readable_ports =
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
         converter does not suppor this."

let of_ir ir ~user_sendable_ports ~user_readable_ports =
  let n = Ir.Chp.unwrap ir in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in
  let%map.Result () = check_n n ~user_sendable_ports ~user_readable_ports in

  let next_v_id = ref 0 in
  let var_of_var = Ir.Var.U.Table.create () in
  let new_var bitwidth =
    let id = !next_v_id in
    incr next_v_id;
    { Var.id; bitwidth }
  in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout v.d.dtype with Bits_fixed bitwidth -> bitwidth
        in
        new_var bitwidth)
  in

  let next_c_id = ref 0 in
  let chan_of_chan = Ir.Chan.U.Table.create () in
  let new_chan bitwidth =
    let id = !next_c_id in
    incr next_c_id;
    { Chan.id; bitwidth }
  in
  let of_c c =
    Hashtbl.find_or_add chan_of_chan c ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout c.d.dtype with Bits_fixed bitwidth -> bitwidth
        in

        new_chan bitwidth)
  in

  let of_e e =
    let rec f e =
      match e with
      | Ir.Expr.K.Add (a, b) -> Expr.Add (f a, f b)
      | Sub_no_wrap (a, b) -> Sub_no_wrap (f a, f b)
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
      | With_assert_log (_, v, _, _) -> f v
      | With_assert_log_fn (_, _, v) -> f v
    in
    f e.Ir.Expr.k
  in

  let mems_table = Ir.Mem.Table.create () in
  let chans_of_mem mem =
    Hashtbl.find_or_add mems_table mem ~default:(fun () ->
        let idx_bits = mem.d.init |> Array.length |> Int.ceil_log2 in
        let cell_bits =
          match Ir.DType.layout mem.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        ( new_chan (1 + idx_bits),
          new_chan cell_bits,
          new_chan cell_bits,
          idx_bits,
          cell_bits ))
  in

  let rec of_n n =
    match n with
    | Ir.Chp.Par (_, ns) -> Chp_stmt.Par (List.map ns ~f:of_n)
    | Seq (_, ns) -> Seq (List.map ns ~f:of_n)
    | Loop (_, n) -> DoWhile (of_n n, Expr.Const CInt.one)
    | SelectImm (_, branches, else_) ->
        let guards = List.map branches ~f:(fun (guard, _) -> of_e guard) in
        let else_guard =
          Option.map else_ ~f:(fun _ ->
              let any_guard_true =
                List.reduce guards ~f:(fun a b -> Expr.BitOr (a, b))
                |> Option.value ~default:(Expr.Const CInt.zero)
              in
              Expr.BitXor (any_guard_true, Const CInt.one))
        in
        let guards = guards @ Option.to_list else_guard in
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
        SelectImm (guard_expr, stmts)
    | WhileLoop (_, expr, seq) ->
        let guard_expr = Expr.Add (of_e expr, Const CInt.one) in
        Seq [ SelectImm (guard_expr, [ Nop; DoWhile (of_n seq, of_e expr) ]) ]
    | DoWhile (_, seq, expr) -> DoWhile (of_n seq, of_e expr)
    | Assign (_, id, expr) -> Assign (of_v id, of_e expr)
    | Send (_, chan, expr) -> Send (of_c chan, of_e expr)
    | Read (_, chan, var) -> Read (of_c chan, of_v var)
    | Log _ | Log1 _ | Assert _ -> (* For now, ignore these nodes *) Nop
    | Nop -> Nop
    | ReadUGMem (_, mem, idx, dst) ->
        let cmd_chan, _, read_chan, _, _ = chans_of_mem mem in
        Par
          [
            Send (cmd_chan, LShift (of_e idx, Const CInt.one));
            Read (read_chan, of_v dst);
          ]
    | WriteUGMem (_, mem, idx, value) ->
        let cmd_chan, write_chan, _, _, _ = chans_of_mem mem in
        Par
          [
            Send
              ( cmd_chan,
                BitOr (LShift (of_e idx, Const CInt.one), Const CInt.one) );
            Send (write_chan, of_e value);
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
           Chp_stmt.Assign (var_id, Const init))
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
  |> Result.map_error ~f:Error.of_string
