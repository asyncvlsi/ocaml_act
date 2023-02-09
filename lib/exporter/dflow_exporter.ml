open! Core
open! Act
module Ir = Internal_rep

type t = string
(* At the moment, we only support top level exporting. In addition, no line information
   is retained. If invalid code is generated, that is a bug of this code generator *)

let is_dflowable n ~user_sendable_ports ~user_readable_ports =
  (* assume n is a top-level statement. For now, we will just unilaterally impose
     dflow symatics. TODO require a chp node to weaken synmatics. *)

  (* First check there are no unsupported nodes *)
  let rec check_sup_nodes n =
    match n with
    | Ir.N.Par (_, ns) -> List.iter ns ~f:check_sup_nodes
    | Seq (_, ns) -> List.iter ns ~f:check_sup_nodes
    | Loop (_, n) -> check_sup_nodes n
    | SelectImm (_, branches, else_) ->
        List.iter branches ~f:(fun (_, n) -> check_sup_nodes n);
        Option.iter else_ ~f:check_sup_nodes
    | WhileLoop (_, _, seq) -> check_sup_nodes seq
    | Assign (_, _, _) -> ()
    | Send (_, _, _) -> ()
    | Read (_, _, _) -> ()
    | Nop -> ()
    (* Factor these into a seperate process? *)
    | ReadUGMem (_, _, _, _) -> failwith "TODO - ReadUGMem not supported"
    | WriteUGMem (_, _, _, _) -> failwith "TODO - WriteUGMem not supported"
    (* These cant be supported in dflow *)
    | WaitUntilReadReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilReadReady"
    | WaitUntilSendReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilSendReady"
    (* Should these be supported, or should they just be dropped? *)
    | Log _ | Log1 _ | Assert _ -> ()
  in
  check_sup_nodes n;

  (* check that par branches dont both use the same side of the same channel *)
  let rec chans n ~r ~w =
    let f n = chans n ~r ~w in
    match n with
    | Ir.N.Par (_, ns) -> List.concat_map ns ~f
    | Seq (_, ns) -> List.concat_map ns ~f
    | Loop (_, n) -> f n
    | SelectImm (_, branches, else_) -> (
        List.concat_map branches ~f:(fun (_, n) -> f n)
        @ match else_ with Some else_ -> f else_ | None -> [])
    | WhileLoop (_, _, seq) -> f seq
    | Assign (_, _, _) -> []
    | Send (_, chan, _) -> if w then [ chan ] else []
    | Read (_, chan, _) -> if r then [ chan ] else []
    | Nop -> []
    | ReadUGMem (_, _, _, _) ->
        failwith "TODO - dflow does not support mem - ReadUGMem"
    | WriteUGMem (_, _, _, _) ->
        failwith "TODO - dflow does not support mem - WriteUGMem"
    | WaitUntilReadReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilReadReady"
    | WaitUntilSendReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilSendReady"
    | Log _ | Log1 _ | Assert _ -> []
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
    | Ir.N.Par (_, ns) -> (
        List.iter ns ~f:check_par_nodes;
        (match find_conflicting_pair ns ~f:r_chans with
        | None -> ()
        | Some (_, _, _) ->
            failwith
              "Two branches of par block read the same channel. Dataflow \
               converter does not suppor this");
        match find_conflicting_pair ns ~f:w_chans with
        | None -> ()
        | Some (_, _, _) ->
            failwith
              "Two branches of par block write the same channel. Dataflow \
               converter does not suppor this.")
    | Seq (_, ns) -> List.iter ns ~f:check_par_nodes
    | Loop (_, n) -> check_par_nodes n
    | SelectImm (_, branches, else_) ->
        List.iter branches ~f:(fun (_, n) -> check_par_nodes n);
        Option.iter else_ ~f:check_par_nodes
    | WhileLoop (_, _, seq) -> check_par_nodes seq
    | Assign (_, _, _) -> ()
    | Send (_, _, _) -> ()
    | Read (_, _, _) -> ()
    | Nop -> ()
    (* Factor these into a seperate process? *)
    | ReadUGMem (_, _, _, _) -> failwith "TODO - ReadUGMem not supported"
    | WriteUGMem (_, _, _, _) -> failwith "TODO - WriteUGMem not supported"
    (* These cant be supported in dflow *)
    | WaitUntilReadReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilReadReady"
    | WaitUntilSendReady (_, _) ->
        failwith "dflow does not support probes but has WaitUntilSendReady"
    (* Should these be supported, or should they just be dropped? *)
    | Log _ | Log1 _ | Assert _ -> ()
  in
  check_par_nodes n;

  (* Then check that each io channel is not also read/written in the program. *)
  (match Set.inter user_readable_ports (r_chans n) |> Set.to_list with
  | [] -> ()
  | _ :: _ ->
      failwith
        "Channel read in prgram but listed as user readable.. Dataflow \
         converter does not suppor this.");
  match Set.inter user_sendable_ports (w_chans n) |> Set.to_list with
  | [] -> ()
  | _ :: _ ->
      failwith
        "Channel written in prgram but listed as user sendable. Dataflow \
         converter does not suppor this."

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

module Simple_IR = struct
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
end

let to_simple_ir n =
  let next_v_id = ref 0 in
  let var_of_var = Ir.Var.U.Table.create () in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () ->
        let id = !next_v_id in
        incr next_v_id;
        let bitwidth =
          match Ir.DType.layout v.d.dtype with Bits_fixed bitwidth -> bitwidth
        in
        { Var.id; bitwidth })
  in

  let next_c_id = ref 0 in
  let chan_of_chan = Ir.Chan.U.Table.create () in
  let of_c c =
    Hashtbl.find_or_add chan_of_chan c ~default:(fun () ->
        let id = !next_c_id in
        incr next_c_id;
        let bitwidth =
          match Ir.DType.layout c.d.dtype with Bits_fixed bitwidth -> bitwidth
        in
        { Chan.id; bitwidth })
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
  let rec of_n n =
    match n with
    | Ir.N.Par (_, ns) -> Simple_IR.Par (List.map ns ~f:of_n)
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
    | Assign (_, id, expr) -> Assign (of_v id, of_e expr)
    | Send (_, chan, expr) -> Send (of_c chan, of_e expr)
    | Read (_, chan, var) -> Read (of_c chan, of_v var)
    | Log _ | Log1 _ | Assert _ -> (* For now, ignore these nodes *) Nop
    | Nop -> Nop
    | ReadUGMem _ | WriteUGMem _ | WaitUntilReadReady _ | WaitUntilSendReady _
      ->
        failwith "TODO - exporter bug. Should have checked this in is_dflowable"
  in
  let n = of_n n in
  (* Then add on all the initializers *)
  (* This map conversion makes the order deterministic *)
  let inits =
    Hashtbl.to_alist var_of_var
    |> Ir.Var.U.Map.of_alist_exn |> Map.to_alist
    |> List.filter_map ~f:(fun (var, var_id) ->
           Option.map var.d.init ~f:(fun init ->
               let init = Ir.DType.cint_of_value var.d.dtype init in
               Simple_IR.Assign (var_id, Const init)))
  in
  Simple_IR.Seq [ Seq inits; n ]

module STF = struct
  module Par_split = struct
    type t = { in_v : Var.t; out_vs : Var.t option list } [@@deriving sexp]
  end

  module Par_merge = struct
    type t = { in_vs : Var.t option list; out_v : Var.t } [@@deriving sexp]
  end

  module DoWhile_phi = struct
    type t = {
      init_v : Var.t option;
      body_in_v : Var.t option;
      body_out_v : Var.t option;
      out_v : Var.t option;
    }
    [@@deriving sexp]
  end

  module Select_split = struct
    type t = { in_v : Var.t; out_vs : Var.t option list } [@@deriving sexp]
  end

  module Select_merge = struct
    type t = { in_vs : Var.t list; out_v : Var.t } [@@deriving sexp]
  end

  type t =
    | Nop
    | Assign of Var.t * Var.t Expr.t
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t Expr.t
    | Seq of t list
    | Par of Par_split.t list * t list * Par_merge.t list
    | SelectImm of
        Var.t Expr.t * Select_split.t list * t list * Select_merge.t list
    | DoWhile of DoWhile_phi.t list * t * Var.t Expr.t
  [@@deriving sexp]
end

let to_stf n =
  (* then put the program in standard token form *)
  let next_id = ref 0 in
  let rec of_stmt stmt ~id_of_id ~raw_read_id_of_id =
    (* let id_of_id = Var.Table.create () in  *)
    let of_v' v ~id_of_id ~raw_read_id_of_id =
      match Hashtbl.find id_of_id v with
      | Some id -> id
      | None ->
          Hashtbl.find_or_add raw_read_id_of_id v ~default:(fun () ->
              let id = !next_id in
              incr next_id;
              { Var.id; bitwidth = v.Var.bitwidth })
    in
    let of_v v = of_v' v ~id_of_id ~raw_read_id_of_id in

    let write_v' v ~id_of_id =
      let id = !next_id in
      incr next_id;
      let data = { Var.id; bitwidth = v.Var.bitwidth } in
      Hashtbl.set id_of_id ~key:v ~data;
      data
    in
    let write_v v = write_v' v ~id_of_id in

    let of_e' e ~id_of_id ~raw_read_id_of_id =
      let rec f e =
        match e with
        | Expr.Add (a, b) -> Expr.Add (f a, f b)
        | Sub_no_wrap (a, b) -> Sub_no_wrap (f a, f b)
        | Mul (a, b) -> Mul (f a, f b)
        | Div (a, b) -> Div (f a, f b)
        | Mod (a, b) -> Mod (f a, f b)
        | LShift (a, b) -> LShift (f a, f b)
        | RShift (a, b) -> RShift (f a, f b)
        | BitAnd (a, b) -> BitAnd (f a, f b)
        | BitOr (a, b) -> BitOr (f a, f b)
        | BitXor (a, b) -> BitXor (f a, f b)
        | Eq (a, b) -> Eq (f a, f b)
        | Ne (a, b) -> Ne (f a, f b)
        | Lt (a, b) -> Lt (f a, f b)
        | Le (a, b) -> Le (f a, f b)
        | Gt (a, b) -> Gt (f a, f b)
        | Ge (a, b) -> Ge (f a, f b)
        | Var v -> Var (of_v' v ~id_of_id ~raw_read_id_of_id)
        | Clip (e, bits) -> Clip (f e, bits)
        | Const c -> Const c
      in
      f e
    in
    let of_e e = of_e' e ~id_of_id ~raw_read_id_of_id in

    match stmt with
    | Simple_IR.Nop -> STF.Nop
    | Assign (v, e) -> Assign (write_v v, of_e e)
    | Send (c, e) -> Send (c, of_e e)
    | Read (c, v) -> Read (c, write_v v)
    | Seq stmts ->
        let stmts =
          List.map stmts ~f:(fun stmt ->
              of_stmt stmt ~id_of_id ~raw_read_id_of_id)
        in
        Seq stmts
    | Par stmts ->
        let l =
          List.map stmts ~f:(fun stmt ->
              let id_of_id = Var.Table.create () in
              let raw_read_id_of_id = Var.Table.create () in
              let stmt = of_stmt stmt ~id_of_id ~raw_read_id_of_id in
              (stmt, id_of_id, raw_read_id_of_id))
        in
        let stmts = List.map l ~f:(fun (stmt, _, _) -> stmt) in
        let write_ids =
          List.concat_map l ~f:(fun (_, id_of_id, _) -> Hashtbl.keys id_of_id)
          |> Var.Set.of_list
        in
        let merges =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l ~f:(fun (_, id_of_id, _) ->
                       Hashtbl.find id_of_id write_id)
                 in
                 let out_v = write_v write_id in
                 { STF.Par_merge.in_vs; out_v })
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, raw_read_id_of_id) ->
              Hashtbl.keys raw_read_id_of_id)
          |> Var.Set.of_list
        in
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, raw_read_id_of_id) ->
                       Hashtbl.find raw_read_id_of_id raw_read_id)
                 in
                 { STF.Par_split.in_v; out_vs })
        in
        Par (splits, stmts, merges)
    | SelectImm (gaurd_expr, branches) ->
        let gaurd_expr = of_e gaurd_expr in
        let l =
          List.map branches ~f:(fun stmt ->
              let id_of_id = Var.Table.create () in
              let raw_read_id_of_id = Var.Table.create () in
              let stmt = of_stmt stmt ~id_of_id ~raw_read_id_of_id in
              (stmt, id_of_id, raw_read_id_of_id))
        in
        let branches = List.map l ~f:(fun (branch, _, _) -> branch) in
        let write_ids =
          List.concat_map l ~f:(fun (_, id_of_id, _) -> Hashtbl.keys id_of_id)
          |> Var.Set.of_list
        in
        (* every branch must either write the write_id or have it as a raw read. If not, add it as a raw read *)
        let merges =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l ~f:(fun (_, id_of_id, raw_read_id_of_id) ->
                       of_v' write_id ~id_of_id ~raw_read_id_of_id)
                 in
                 let out_v = write_v write_id in
                 { STF.Select_merge.in_vs; out_v })
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, raw_read_id_of_id) ->
              Hashtbl.keys raw_read_id_of_id)
          |> Var.Set.of_list
        in
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, raw_read_id_of_id) ->
                       Hashtbl.find raw_read_id_of_id raw_read_id)
                 in
                 { STF.Select_split.in_v; out_vs })
        in
        SelectImm (gaurd_expr, splits, branches, merges)
    | DoWhile (stmt, guard) ->
        let id_of_id' = Var.Table.create () in
        let raw_read_id_of_id' = Var.Table.create () in
        let stmt =
          of_stmt stmt ~id_of_id:id_of_id' ~raw_read_id_of_id:raw_read_id_of_id'
        in
        let guard =
          of_e' guard ~id_of_id:id_of_id' ~raw_read_id_of_id:raw_read_id_of_id'
        in
        let phis =
          Hashtbl.keys id_of_id' @ Hashtbl.keys raw_read_id_of_id'
          |> Var.Set.of_list |> Set.to_list
          |> List.map ~f:(fun var_id ->
                 let body_read = Hashtbl.find raw_read_id_of_id' var_id in
                 let body_write = Hashtbl.find id_of_id' var_id in
                 match (body_read, body_write) with
                 | None, None -> failwith "unreachable"
                 | Some body_read, None ->
                     let init_v = of_v var_id in
                     {
                       STF.DoWhile_phi.init_v = Some init_v;
                       body_in_v = Some body_read;
                       body_out_v = Some body_read;
                       out_v = None;
                     }
                 | None, Some body_write ->
                     (* let init_v = of_v var_id in  *)
                     let out_v = write_v var_id in
                     {
                       init_v = None;
                       body_in_v = None;
                       body_out_v = Some body_write;
                       out_v = Some out_v;
                     }
                 | Some body_read, Some body_write ->
                     let init_v = of_v var_id in
                     let out_v = write_v var_id in
                     {
                       init_v = Some init_v;
                       body_in_v = Some body_read;
                       body_out_v = Some body_write;
                       out_v = Some out_v;
                     })
        in
        DoWhile (phis, stmt, guard)
  in
  let id_of_id = Var.Table.create () in
  let raw_read_id_of_id = Var.Table.create () in
  let n = of_stmt n ~id_of_id ~raw_read_id_of_id in
  assert (Hashtbl.is_empty raw_read_id_of_id);
  n

let optimize_stf n =
  (* flatten the generated code. TODO flatten par blocks *)
  let rec flatten n =
    match n with
    | STF.Nop -> STF.Nop
    | Assign (v, e) -> Assign (v, e)
    | Send (c, e) -> Send (c, e)
    | Read (c, v) -> Read (c, v)
    | Seq stmts -> (
        let stmts =
          List.concat_map stmts ~f:(fun stmt ->
              let stmt = flatten stmt in
              match stmt with Nop -> [] | Seq stmts -> stmts | _ -> [ stmt ])
        in
        match stmts with [] -> Nop | [ stmt ] -> stmt | stmts -> Seq stmts)
    | Par (splits, stmts, merges) -> (
        match stmts with
        | [] ->
            (* cant write variables if you have no code inside the par block *)
            assert (List.is_empty merges);
            Nop
        | [ stmt ] ->
            let copy_ins =
              List.filter_map splits ~f:(fun split ->
                  assert (Int.equal 1 (List.length split.out_vs));
                  List.hd_exn split.out_vs
                  |> Option.map ~f:(fun out_v ->
                         STF.Assign (out_v, Var split.in_v)))
            in
            let copy_outs =
              List.filter_map merges ~f:(fun merge ->
                  assert (Int.equal 1 (List.length merge.in_vs));
                  List.hd_exn merge.in_vs
                  |> Option.map ~f:(fun in_v ->
                         STF.Assign (merge.out_v, Var in_v)))
            in
            Seq (copy_ins @ [ stmt ] @ copy_outs)
        | stmts ->
            let stmts = List.map stmts ~f:flatten in
            (* TODO flatten nested par statements *)
            Par (splits, stmts, merges))
    | SelectImm (splits, gaurd_var, branches, merges) ->
        let branches = List.map branches ~f:flatten in
        SelectImm (splits, gaurd_var, branches, merges)
    | DoWhile (phis, stmt, guard) ->
        let stmt = flatten stmt in
        DoWhile (phis, stmt, guard)
  in
  flatten n

module Dflow_id = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Dflow = struct
  type t =
    | Assign of Dflow_id.t * Dflow_id.t Expr.t
    | Split of Dflow_id.t * Dflow_id.t * Dflow_id.t option list
    | Merge of Dflow_id.t * Dflow_id.t list * Dflow_id.t
    | Copy_init of Dflow_id.t * Dflow_id.t * CInt.t
    | Ctrl_send of Dflow_id.t
    | Ctrl_read of Dflow_id.t
    | Ctrl_seq_send of
        Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
    | Ctrl_seq_read of
        Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
    | Ctrl_select_send of
        Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * (Dflow_id.t * Dflow_id.t) option list
    | Ctrl_select_read of
        Dflow_id.t
        * Dflow_id.t
        * Dflow_id.t
        * (Dflow_id.t * Dflow_id.t) option list
    | Ctrl_do_while_send of
        Dflow_id.t * Dflow_id.t * Dflow_id.t * Dflow_id.t * Dflow_id.t
    | Ctrl_do_while_read of
        Dflow_id.t * Dflow_id.t * Dflow_id.t * Dflow_id.t * Dflow_id.t
  [@@deriving sexp]
end

module Chan_end = struct
  module T = struct
    type t = Read of Chan.t | Send of Chan.t
    [@@deriving sexp, equal, hash, compare]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

let map_merge_skew_and_accum_list m1 m2 ~combine =
  let l = Queue.create () in
  let combine ~key v1 v2 =
    let v, a = combine ~key v1 v2 in
    Queue.enqueue l a;
    v
  in
  let m = Map.merge_skewed m1 m2 ~combine in
  (m, Queue.to_list l)

let to_dflow n =
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Dflow_id.id; bitwidth }
  in
  let new_alias (a : Dflow_id.t) = new_chan a.bitwidth in
  let new_ctrl () = new_chan 1 in

  (* control processes from rui's paper (converted into dataflow and adapted a bit). *)
  (* let dflow_ctrl_send ctrl =
       let s_ = new_ctrl () in
       [ Dflow.Copy_init (ctrl, s_, CInt.zero); Assign (s_, BitXor (Const CInt.one, Var ctrl))]
     in
  *)
  let id_of_var = Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add id_of_var v ~default:(fun () -> new_chan v.bitwidth)
  in

  let id_of_chan = Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add id_of_chan c ~default:(fun () -> new_chan c.bitwidth)
  in

  let of_e e = Expr.map_vars e ~f:(fun v -> of_v v) in
  let of_e' (e : Var.t Expr.t) =
    let bitwidth = Expr.bitwidth e ~bits_of_var:(fun v -> v.bitwidth) in
    let v = new_chan bitwidth in
    (Dflow.Assign (v, of_e e), v)
  in
  (* returns a tuple (dflow, alias_map) where alias_map is a map { Chan_end.t -> (alias, ctrl)} *)
  let rec of_stmt n =
    match n with
    | STF.Nop -> ([], Chan_end.Map.empty)
    | Assign (v, e) -> ([ Dflow.Assign (of_v v, of_e e) ], Chan_end.Map.empty)
    | Send (c, e) ->
        let cc = of_c c in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Send c) (cc, ctrl) in
        let dflow = [ Dflow.Assign (cc, of_e e); Ctrl_send ctrl ] in
        (dflow, alias_map)
    | Read (c, v) ->
        let cc = of_c c in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Read c) (cc, ctrl) in
        let dflow = [ Dflow.Assign (of_v v, Var (of_c c)); Ctrl_read ctrl ] in
        (dflow, alias_map)
    | Seq stmts ->
        List.map stmts ~f:of_stmt
        |> List.fold ~init:([], Chan_end.Map.empty)
             ~f:(fun (dflow, alias_map) (dflow', alias_map') ->
               (* fold together the alias maps and add needed control processes *)
               (* If an alias is in only one map, just copy it up. If an alias is in both maps, we must merge them with a control process *)
               let alias_map, ctrl_dflow =
                 map_merge_skew_and_accum_list alias_map alias_map'
                   ~combine:(fun ~key (a1, b1) (a2, b2) ->
                     assert (Int.equal a1.bitwidth a2.bitwidth);
                     let a_chan = new_alias a1 in
                     let b_chan = new_ctrl () in
                     let ctrl_proc =
                       match key with
                       | Chan_end.Send _ ->
                           Dflow.Ctrl_seq_send (a_chan, b_chan, a1, b1, a2, b2)
                       | Read _ -> Ctrl_seq_read (a_chan, b_chan, a1, b1, a2, b2)
                     in
                     ((a_chan, b_chan), ctrl_proc))
               in
               (dflow @ dflow' @ ctrl_dflow, alias_map))
    | Par (splits, stmts, merges) ->
        let splits =
          List.concat_map splits ~f:(fun split ->
              List.filter_map split.out_vs ~f:Fn.id
              |> List.map ~f:(fun out_v ->
                     Dflow.Assign (of_v out_v, Var (of_v split.in_v))))
        in
        let merges =
          List.concat_map merges ~f:(fun merge ->
              List.filter_map merge.in_vs ~f:Fn.id
              |> List.map ~f:(fun in_v ->
                     Dflow.Assign (of_v merge.out_v, Var (of_v in_v))))
        in
        let stmts = List.map stmts ~f:of_stmt in
        let alias_map =
          List.map stmts ~f:snd
          |> List.reduce ~f:(fun m1 m2 ->
                 Map.merge_skewed m1 m2 ~combine:(fun ~key:_ _ _ ->
                     failwith
                       "Multiple branches interact with the same end of the \
                        same channel. This is a bug. It should have been \
                        cought in is_dflowable"))
          |> Option.value ~default:Chan_end.Map.empty
        in
        let stmts = List.concat_map stmts ~f:fst in
        let stmts = List.concat [ splits; merges; stmts ] in
        (stmts, alias_map)
    | SelectImm (guard_expr, splits, branches, merges) ->
        let guard_proc, guard_expr = of_e' guard_expr in
        let splits =
          List.map splits ~f:(fun split ->
              let out_vs = List.map split.out_vs ~f:(Option.map ~f:of_v) in
              Dflow.Split (guard_expr, of_v split.in_v, out_vs))
        in
        let merges =
          List.map merges ~f:(fun merge ->
              let in_vs = List.map merge.in_vs ~f:of_v in
              Dflow.Merge (guard_expr, in_vs, of_v merge.out_v))
        in
        let branches = List.map branches ~f:of_stmt in
        let alias_map, ctrl_procs =
          let keys =
            List.concat_map branches ~f:(fun (_, alias_map) ->
                Map.keys alias_map)
            |> Chan_end.Set.of_list |> Set.to_list
          in
          let l =
            List.map keys ~f:(fun k ->
                let aliases =
                  List.map branches ~f:(fun (_, alias_map) ->
                      Map.find alias_map k)
                in
                let alias_bitwidth =
                  List.filter_opt aliases
                  |> List.map ~f:(fun (alias, _) -> alias.bitwidth)
                  |> List.all_equal ~equal:Int.equal
                  |> Option.value_exn
                in
                (* rewrite the gaurd expression to be an index into a list. This removes the need for an "else" case. *)
                let a_chan = new_chan alias_bitwidth in
                let b_chan = new_ctrl () in
                let ctrl_proc =
                  match k with
                  | Send _ ->
                      Dflow.Ctrl_select_send
                        (a_chan, b_chan, guard_expr, aliases)
                  | Read _ ->
                      Ctrl_select_read (a_chan, b_chan, guard_expr, aliases)
                in
                ((k, (a_chan, b_chan)), ctrl_proc))
          in
          let alias_list, ctrl_procs = List.unzip l in
          (Chan_end.Map.of_alist_exn alias_list, ctrl_procs)
        in
        let branches = List.concat_map branches ~f:fst in
        let dflow =
          List.concat [ [ guard_proc ]; splits; merges; branches; ctrl_procs ]
        in
        (dflow, alias_map)
    | DoWhile (phis, stmt, guard) ->
        let guard_proc, guard = of_e' guard in
        let prev_guard = new_alias guard in
        let phis =
          List.concat_map phis ~f:(fun phi ->
              let bitwidth =
                [ phi.init_v; phi.body_in_v; phi.body_out_v; phi.out_v ]
                |> List.filter_opt
                |> List.map ~f:(fun v -> v.bitwidth)
                |> List.all_equal ~equal:Int.equal
                |> Option.value_exn
              in
              let carry = new_chan bitwidth in
              let top =
                Option.map phi.body_in_v ~f:(fun body_in_v ->
                    let init_v = Option.value_exn phi.init_v |> of_v in
                    Dflow.Merge (prev_guard, [ init_v; carry ], of_v body_in_v))
              in
              let bottom =
                Option.map phi.body_out_v ~f:(fun body_out_v ->
                    let out_vs = [ Option.map phi.out_v ~f:of_v; Some carry ] in
                    Dflow.Split (guard, of_v body_out_v, out_vs))
              in
              let copy_init = Dflow.Copy_init (prev_guard, guard, CInt.zero) in
              List.filter_opt [ top; bottom; Some copy_init ])
        in
        let stmts, alias_map = of_stmt stmt in
        let alias_map, ctrl_procs =
          let m =
            Map.mapi alias_map ~f:(fun ~key ~data:(alias, ctrl) ->
                let a_chan = new_alias alias in
                let b_chan = new_ctrl () in
                let ctrl_proc =
                  match key with
                  | Send _ ->
                      Dflow.Ctrl_do_while_send
                        (a_chan, b_chan, guard, alias, ctrl)
                  | Read _ ->
                      Ctrl_do_while_read (a_chan, b_chan, guard, alias, ctrl)
                in
                ((a_chan, b_chan), ctrl_proc))
          in
          let l = Map.data m |> List.map ~f:snd in
          let m = Map.map m ~f:fst in
          (m, l)
        in
        let dflow = (guard_proc :: phis) @ stmts @ ctrl_procs in
        (dflow, alias_map)
  in
  let dflow, _alias_map = of_stmt n in
  dflow

let optimize_dflow dflow = dflow

let create ir ~user_sendable_ports ~user_readable_ports =
  let ir = Ir.N.unwrap ir in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);

  is_dflowable ir ~user_sendable_ports ~user_readable_ports;

  (* From this point on, we should not fail! *)
  let n = to_simple_ir ir in
  let n = to_stf n in
  let n = optimize_stf n in
  let dflow = to_dflow n in
  let dflow = optimize_dflow dflow in

  (* now export as dataflow code *)
  print_s [%sexp (n : STF.t)];
  print_s [%sexp (dflow : Dflow.t list)];

  (* print_s [%sexp (chan_remaps: STF.t)]; *)
  (* print_s [%sexp (ctrl_procs: STF.t)]; *)
  (* next, add on helper processes to manage *)
  ""

(*
   let ctrl_procs =
     List.concat_map ctrl_procs ~f:(fun proc ->
         match proc with
         | `Read ctrl_chan -> []
         | `Send ctrl_chan ->
             let ctrl =
               WhileLoop
                 ( Const CInt.one,
                   Seq
                     [ Send (ctrl_chan, CInt.zero); Send (ctrl_chan, CInt.one) ]
                 )
             in
             [ ctrl ]
         | `Seq_send (a_chan, b_chan, a1, b1, a2, b2) ->
             let s = failwith "TODO" in
             let b = failwith "TODO" in
             let not_ v = Expr.BitXor (Var v, Const CInt.one) in
             let ctrl_init = Assign (b, CInt.zero) in
             let ctrl_loop =
               WhileLoop
                 ( Const CInt.one,
                   Seq
                     [
                       SelectImm ([ (Var s, Read (b2, b)) ], Some (Read (b1, b)));
                       SelectImm
                         ( [
                             (BitAnd (not_ s, Var b), Send (a1, x));
                             (BitAnd (Var s, Var b), Send (a2, x));
                           ],
                           Some (Assign (s, not_ s)) );
                       SelectImm
                         ( [ (BitAnd (Var s, not_ b), Nop) ],
                           Some (Send (b_chan, b)) );
                       SelectImm ([ (Var b, Send (A, x)) ], Some Nop);
                     ] )
             in
             [ Seq [ ctrl_init; ctrl_loop ] ]
         | `Seq_read (a_chan, b_chan, a1, b1, a2, b2) -> []
         | `Loop_send (c0_chan, c1_chan, c_chan, a_chan, b_chan, a, b) ->
             let s = failwith "TODO" in
             let b = failwith "TODO" in
             let g = failwith "TODO" in
             let not_ v = Expr.BitXor (Var v, Const CInt.one) in
             let ctrl_init = Assign (b, CInt.zero) in
             let ctrl_loop =
               WhileLoop
                 ( Const CInt.one,
                   Seq
                     [
                       SelectImm ([ (Var s, Read (b2, b)) ], Some (Read (b1, b)));
                       SelectImm
                         ( [
                             (BitAnd (not_ s, Var b), Send (a1, x));
                             (BitAnd (Var s, Var b), Send (a2, x));
                           ],
                           Some (Assign (s, not_ s)) );
                       SelectImm
                         ( [ (BitAnd (Var s, not_ b), Nop) ],
                           Some (Send (b_chan, b)) );
                       SelectImm ([ (Var b, Send (A, x)) ], Some Nop);
                     ] )
             in
             [ Seq [ ctrl_init; ctrl_loop ] ]
         | `Loop_read (c0_chan, c1_chan, c_chan, a_chan, b_chan, a, b) -> []
         | `Select_send (a_chan, b_chan, c_chan, branch_abcs, else_abc) -> []
         | `Select_read (a_chan, b_chan, c_chan, branch_abcs, else_abc) -> []) *)
