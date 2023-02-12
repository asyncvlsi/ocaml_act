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
    | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_sup_nodes seq
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
    | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> f seq
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
    | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_par_nodes seq
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
  (* TODO also check variables not used same side of par node *)
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
    [@@deriving sexp_of]
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
    | DoWhile (_, seq, expr) -> DoWhile (of_n seq, of_e expr)
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
  (Simple_IR.Seq [ Seq inits; n ], chan_of_chan)

module STF_Var = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module STF = struct
  module Par_split = struct
    type t = { in_v : STF_Var.t; out_vs : STF_Var.t option list } [@@deriving sexp]
  end

  module Par_merge = struct
    type t = { in_vs : STF_Var.t option list; out_v : STF_Var.t } [@@deriving sexp]
  end

  module DoWhile_phi = struct
    type t = {
      init_v : STF_Var.t option;
      body_in_v : STF_Var.t option;
      body_out_v : STF_Var.t option;
      out_v : STF_Var.t option;
    }
    [@@deriving sexp]
  end

  module Select_split = struct
    type t = { in_v : STF_Var.t; out_vs : STF_Var.t option list } [@@deriving sexp]
  end

  module Select_merge = struct
    type t = { in_vs : STF_Var.t list; out_v : STF_Var.t } [@@deriving sexp]
  end

  type t =
    | Nop
    | Assign of STF_Var.t * STF_Var.t Expr.t
    | Read of Chan.t * STF_Var.t
    | Send of Chan.t * STF_Var.t Expr.t
    | Seq of t list
    | Par of Par_split.t list * t list * Par_merge.t list
    | SelectImm of
        STF_Var.t Expr.t * Select_split.t list * t list * Select_merge.t list
    | DoWhile of DoWhile_phi.t list * t * STF_Var.t Expr.t
  [@@deriving sexp]
end

let to_stf n =
  (* then put the program in standard token form *)
  let next_id = ref 0 in
  let rec of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id =
    let of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id =
      match Hashtbl.find stf_id_of_id v with
      | Some id -> id
      | None ->
          Hashtbl.find_or_add stf_id_of_raw_read_id v ~default:(fun () ->
              let id = !next_id in
              incr next_id;
              { STF_Var.id; bitwidth = v.Var.bitwidth })
    in
    let of_v v = of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id in

    let write_v' v ~stf_id_of_id =
      let id = !next_id in
      incr next_id;
      let data = { STF_Var.id; bitwidth = v.Var.bitwidth } in
      Hashtbl.set stf_id_of_id ~key:v ~data;
      data
    in
    let write_v v = write_v' v ~stf_id_of_id in

    let of_e' e ~stf_id_of_id ~stf_id_of_raw_read_id =
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
        | Var v -> Var (of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id)
        | Clip (e, bits) -> Clip (f e, bits)
        | Const c -> Const c
      in
      f e
    in
    let of_e e = of_e' e ~stf_id_of_id ~stf_id_of_raw_read_id in

    match stmt with
    | Simple_IR.Nop -> STF.Nop
    | Assign (v, e) -> Assign (write_v v, of_e e)
    | Send (c, e) -> Send (c, of_e e)
    | Read (c, v) -> Read (c, write_v v)
    | Seq stmts ->
        let stmts =
          List.map stmts ~f:(fun stmt ->
              of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id)
        in
        Seq stmts
    | Par stmts ->
        let l =
          List.map stmts ~f:(fun stmt ->
              let stf_id_of_id = Var.Table.create () in
              let stf_id_of_raw_read_id = Var.Table.create () in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let stmts = List.map l ~f:(fun (stmt, _, _) -> stmt) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) -> Hashtbl.keys stf_id_of_id)
          |> Var.Set.of_list
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
              Hashtbl.keys stf_id_of_raw_read_id)
          |> Var.Set.of_list
        in
        (* print_s [%sexp (("pre_split stf_id_of_raw_read_id", stf_id_of_raw_read_id): string * STF_Var.t Var.Table.t )]; *)
        (* print_s [%sexp (("pre_split stf_id_of_id", stf_id_of_id): string * STF_Var.t Var.Table.t)]; *)
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
                       Hashtbl.find stf_id_of_raw_read_id raw_read_id)
                 in
                 { STF.Par_split.in_v; out_vs })
        in
        let merges =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l ~f:(fun (_, stf_id_of_id, _) ->
                       Hashtbl.find stf_id_of_id write_id)
                 in
                 let out_v = write_v write_id in
                 { STF.Par_merge.in_vs; out_v })
        in
        (* print_s [%sexp (("splits", raw_read_ids, splits): string * Var.Set.t * STF.Par_split.t list)]; *)
        (* print_s [%sexp (("merges", merges): string * STF.Par_merge.t list)]; *)
        Par (splits, stmts, merges)
    | SelectImm (gaurd_expr, branches) ->
        let gaurd_expr = of_e gaurd_expr in
        let l =
          List.map branches ~f:(fun stmt ->
              let stf_id_of_id = Var.Table.create () in
              let stf_id_of_raw_read_id = Var.Table.create () in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let branches = List.map l ~f:(fun (branch, _, _) -> branch) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) -> Hashtbl.keys stf_id_of_id)
          |> Var.Set.of_list
        in
        (* every branch must either write the write_id or have it as a raw read. If not, add it as a raw read *)
        let merges' =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l ~f:(fun (_, stf_id_of_id, stf_id_of_raw_read_id) ->
                       of_v' write_id ~stf_id_of_id ~stf_id_of_raw_read_id)
                 in
                 (write_id, in_vs)
               )
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
              Hashtbl.keys stf_id_of_raw_read_id)
          |> Var.Set.of_list
        in
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
                       Hashtbl.find stf_id_of_raw_read_id raw_read_id)
                 in
                 { STF.Select_split.in_v; out_vs })
        in
        let merges =
          
          List.map merges' ~f:(fun (write_id, in_vs) ->
                 let out_v = write_v write_id in
                 { STF.Select_merge.in_vs; out_v })
        in
        SelectImm (gaurd_expr, splits, branches, merges)
    | DoWhile (stmt, guard) ->
        let stf_id_of_id' = Var.Table.create () in
        let stf_id_of_raw_read_id' = Var.Table.create () in
        let stmt =
          of_stmt stmt ~stf_id_of_id:stf_id_of_id' ~stf_id_of_raw_read_id:stf_id_of_raw_read_id'
        in
        let guard =
          of_e' guard ~stf_id_of_id:stf_id_of_id' ~stf_id_of_raw_read_id:stf_id_of_raw_read_id'
        in
        let phis =
          Hashtbl.keys stf_id_of_id' @ Hashtbl.keys stf_id_of_raw_read_id'
          |> Var.Set.of_list |> Set.to_list
          |> List.map ~f:(fun var_id ->
                 let body_read = Hashtbl.find stf_id_of_raw_read_id' var_id in
                 let body_write = Hashtbl.find stf_id_of_id' var_id in
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
  let stf_id_of_id = Var.Table.create () in
  let stf_id_of_raw_read_id = Var.Table.create () in
  let n = of_stmt n ~stf_id_of_id ~stf_id_of_raw_read_id in
  assert (Hashtbl.is_empty stf_id_of_raw_read_id);
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

  (* TODO *)
  let eliminate_dead_code n = n in

  eliminate_dead_code n |> flatten

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
    | MergeBoolGuard of Dflow_id.t * (Dflow_id.t * Dflow_id.t) * Dflow_id.t
    | SplitBoolGuard of
        Dflow_id.t * Dflow_id.t * (Dflow_id.t option * Dflow_id.t option)
    | Copy_init of Dflow_id.t * Dflow_id.t * CInt.t
    | INode of Ir.Chan.U.t * Dflow_id.t
    | ONode of Ir.Chan.U.t * Dflow_id.t
  [@@deriving sexp_of]
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
  (m, Queue.to_list l |> List.concat)

(* control processes from rui's paper (converted into dataflow and adapted a bit). *)
let rui_ctrl_chan ctrl ~new_chan ~dir:_ =
  let s_ = new_chan 1 in
  [
    Dflow.Copy_init (ctrl, s_, CInt.zero);
    Assign (s_, BitXor (Const CInt.one, Var ctrl));
  ]

let rui_ctrl_send = rui_ctrl_chan ~dir:`Send
let rui_ctrl_read = rui_ctrl_chan ~dir:`Read

let rui_ctrl_seq a b_chan a1 b1 a2 b2 ~new_chan ~dir =
  let x_width = a.Dflow_id.bitwidth in
  let s = new_chan 1 in
  (* [ ~s -> b1?v [] s -> b2?v ] *)
  let v = new_chan 1 in
  let dflow1 = [ Dflow.MergeBoolGuard (s, (b1, b2), v) ] in
  (* [ ~ (s | v) -> skip  []   (s | v) -> b!v   ] *)
  let dflow2 =
    let g = new_chan 1 in
    [
      Dflow.Assign (g, Expr.BitOr (Var s, Var v));
      SplitBoolGuard (g, v, (None, Some b_chan));
    ]
  in
  (* EITHER [ ~v -> s := ~s  []   v -> a?x; [ ~s -> a1!x  []   s -> a2!x ] ]
     OR  [    ~v -> s := ~s  []   v -> [ ~s -> a1?x  []   s -> a2?x ] a!x; ] *)
  let s' = new_chan x_width in
  let dflow3 =
    let s0 = new_chan 1 in
    let s1 = new_chan 1 in
    let s0' = new_chan 1 in
    let op_proc =
      match dir with
      | `Read -> Dflow.SplitBoolGuard (s1, a, (Some a1, Some a2))
      | `Send -> MergeBoolGuard (s1, (a1, a2), a)
    in
    [
      Dflow.SplitBoolGuard (v, s, (Some s0, Some s1));
      Assign (s0', Expr.BitXor (Var s0, Const CInt.one));
      MergeBoolGuard (v, (s0', s1), s');
      op_proc;
    ]
  in
  (* finally, we must wrap `s` back around to the top *)
  let dflow_wrap = [ Dflow.Copy_init (s, s', CInt.zero) ] in
  dflow1 @ dflow2 @ dflow3 @ dflow_wrap

let rui_ctrl_seq_send = rui_ctrl_seq ~dir:`Send
let rui_ctrl_seq_read = rui_ctrl_seq ~dir:`Read

let rui_ctrl_select a_chan b_chan c ab_list ~new_chan ~dir =
  let g_width = c.Dflow_id.bitwidth in
  let v = new_chan 1 in
  let g = new_chan g_width in

  (* [ ~v -> c?g [] v -> skip ] *)
  let g' = new_chan g_width in
  let dflow1 =
    let g1 = new_chan g_width in
    [
      Dflow.SplitBoolGuard (v, g, (None, Some g1));
      MergeBoolGuard (v, (c, g1), g');
    ]
  in

  (* [ g{i}=1   ->   B_i?v ]; B!v; *)
  let v' = new_chan 1 in
  let dflow2 =
    let dflows, vi's =
      List.map ab_list ~f:(fun o ->
          let vi' = new_chan 1 in
          let dflow =
            match o with
            | None -> Dflow.Assign (vi', Const CInt.zero)
            | Some (_, bi) -> Dflow.Assign (vi', Var bi)
          in
          (dflow, vi'))
      |> List.unzip
    in
    List.concat
      [ [ Dflow.Merge (g', vi's, v'); Assign (b_chan, Var v') ]; dflows ]
  in

  (* [~v -> skip []  v -> [ g{i}=1   ->  A0?x ]; A!x ]; OR *)
  (* [~v -> skip []  v -> A?x; [ g{i}=1   ->  A0!x ] ]; OR *)
  let dflow3 =
    let g1' = new_chan g_width in
    let op_procs =
      match dir with
      | `Send ->
          let dflows, ais =
            List.map ab_list ~f:(fun o ->
                let vi' = new_chan 1 in
                let dflow =
                  match o with
                  | None -> Dflow.Assign (vi', Const CInt.zero)
                  | Some (ai, _) -> Dflow.Assign (vi', Var ai)
                in
                (dflow, vi'))
            |> List.unzip
          in
          Dflow.Merge (g1', ais, a_chan) :: dflows
      | `Read ->
          let ais = List.map ab_list ~f:(Option.map ~f:fst) in
          [ Split (g1', a_chan, ais) ]
    in
    Dflow.SplitBoolGuard (v', g', (None, Some g1')) :: op_procs
  in

  (* finally, we must wrap `v` and `g` back around to the top *)
  let dflow_wrap =
    [ Dflow.Copy_init (v, v', CInt.zero); Copy_init (g, g', CInt.one) ]
  in
  dflow1 @ dflow2 @ dflow3 @ dflow_wrap

let rui_ctrl_select_send = rui_ctrl_select ~dir:`Send
let rui_ctrl_select_read = rui_ctrl_select ~dir:`Read

let rui_ctrl_dowhile b_chan guard_chan b1 ~new_chan ~dir:_ =
  let v = new_chan 1 in
  let g = new_chan 1 in
  (* B1?v ;  [ ~v -> C?g  []  v -> skip  ] *)
  let g' = new_chan 1 in
  let dflow1 =
    let g1 = new_chan 1 in
    [
      Dflow.Assign (v, Var b1);
      SplitBoolGuard (v, g, (None, Some g1));
      MergeBoolGuard (v, (guard_chan, g1), g');
    ]
  in
  (* [ ~(v | ~g)  -> skip  []  (v | ~g) -> B!b ]; *)
  let dflow2 =
    let e = new_chan 1 in
    [
      Dflow.Assign (e, BitOr (Var v, BitXor (Var g, Const CInt.one)));
      SplitBoolGuard (e, v, (None, Some b_chan));
    ]
  in
  (* finally, we must wrap `g` back around to the top *)
  let dflow_wrap = [ Dflow.Copy_init (g, g', CInt.zero) ] in
  dflow1 @ dflow2 @ dflow_wrap

let rui_ctrl_dowhile_send = rui_ctrl_dowhile ~dir:`Send
let rui_ctrl_dowhile_read = rui_ctrl_dowhile ~dir:`Send

let to_dflow n ~user_sendable_ports ~user_readable_ports =
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Dflow_id.id; bitwidth }
  in
  let new_alias (a : Dflow_id.t) = new_chan a.bitwidth in
  let new_ctrl () = new_chan 1 in

  let id_of_var = STF_Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add id_of_var v ~default:(fun () -> new_chan v.bitwidth)
  in

  let id_of_chan = Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add id_of_chan c ~default:(fun () -> new_chan c.bitwidth)
  in

  let of_e e = Expr.map_vars e ~f:(fun v -> of_v v) in
  let of_e' (e : STF_Var.t Expr.t) =
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
        let dflow = [ Dflow.Assign (cc, of_e e) ] in
        let ctrl_procs = rui_ctrl_send ctrl ~new_chan in
        let dflow = List.concat [ dflow; ctrl_procs ] in
        (dflow, alias_map)
    | Read (c, v) ->
        let cc = of_c c in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Read c) (cc, ctrl) in
        let dflow = [ Dflow.Assign (of_v v, Var (of_c c)) ] in
        let ctrl_procs = rui_ctrl_read ctrl ~new_chan in
        let dflow = List.concat [ dflow; ctrl_procs ] in
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
                     let ctrl_procs =
                       match key with
                       | Chan_end.Send _ ->
                           rui_ctrl_seq_send a_chan b_chan a1 b1 a2 b2 ~new_chan
                       | Read _ ->
                           rui_ctrl_seq_read a_chan b_chan a1 b1 a2 b2 ~new_chan
                     in
                     ((a_chan, b_chan), ctrl_procs))
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
                let ctrl_procs =
                  match k with
                  | Send _ ->
                      rui_ctrl_select_send a_chan b_chan guard_expr aliases
                        ~new_chan
                  | Read _ ->
                      rui_ctrl_select_read a_chan b_chan guard_expr aliases
                        ~new_chan
                in
                ((k, (a_chan, b_chan)), ctrl_procs))
          in
          let alias_list, ctrl_procs = List.unzip l in
          (Chan_end.Map.of_alist_exn alias_list, List.concat ctrl_procs)
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
                let b_chan = new_ctrl () in
                let ctrl_proc =
                  match key with
                  | Send _ -> rui_ctrl_dowhile_send b_chan guard ctrl ~new_chan
                  | Read _ -> rui_ctrl_dowhile_read b_chan guard ctrl ~new_chan
                in
                ((alias, b_chan), ctrl_proc))
          in
          let l = Map.data m |> List.concat_map ~f:snd in
          let m = Map.map m ~f:fst in
          (m, l)
        in
        let dflow = (guard_proc :: phis) @ stmts @ ctrl_procs in
        (dflow, alias_map)
  in
  let dflow, _alias_map = of_stmt n in
  let inodes =
    Map.to_alist user_sendable_ports
    |> List.map ~f:(fun (ir_chan, chan) -> Dflow.INode (ir_chan, of_c chan))
  in
  let onodes =
    Map.to_alist user_readable_ports
    |> List.map ~f:(fun (ir_chan, chan) -> Dflow.ONode (ir_chan, of_c chan))
  in
  List.concat [ inodes; onodes; dflow ]

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
  let n, user_sendable_ports, user_readable_ports =
    let n, chan_of_chan = to_simple_ir ir in
    let user_sendable_ports =
      Map.of_key_set user_sendable_ports ~f:(fun chan ->
          Hashtbl.find_exn chan_of_chan chan)
    in
    let user_readable_ports =
      Map.of_key_set user_readable_ports ~f:(fun chan ->
          Hashtbl.find_exn chan_of_chan chan)
    in
    (n, user_sendable_ports, user_readable_ports)
  in
  let n = to_stf n in
  let n = optimize_stf n in
  let dflow = to_dflow n ~user_sendable_ports ~user_readable_ports in
  let dflow = optimize_dflow dflow in

  (* now export as dataflow code *)
  print_s [%sexp (n : STF.t)];
  print_s [%sexp (dflow : Dflow.t list)];

  (* print_s [%sexp (chan_remaps: STF.t)]; *)
  (* print_s [%sexp (ctrl_procs: STF.t)]; *)
  (* next, add on helper processes to manage *)
  ""

let simple_ir_sim ir ~user_sendable_ports ~user_readable_ports =
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
  let n, my_chan_of_chan = to_simple_ir ir in
  let user_sendable_ports =
    Map.of_key_set user_sendable_ports ~f:(fun chan ->
        Hashtbl.find_exn my_chan_of_chan chan)
  in
  let user_readable_ports =
    Map.of_key_set user_readable_ports ~f:(fun chan ->
        Hashtbl.find_exn my_chan_of_chan chan)
  in

  let var_of_var = Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () ->
        Act.Var.create (CInt.dtype ~bits:v.bitwidth))
  in

  let chan_of_my_chan = Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add chan_of_my_chan c ~default:(fun () ->
        match
          Hashtbl.to_alist my_chan_of_chan
          |> List.find ~f:(fun (_, my_chan) -> Chan.equal my_chan c)
          |> Option.bind ~f:(fun (ir_chan, _) ->
                 if
                   Map.mem user_readable_ports ir_chan
                   || Map.mem user_sendable_ports ir_chan
                 then (* TODO check bitwidths *)
                   Some ir_chan
                 else None)
        with
        | Some ir_chan -> (ir_chan : Ir.Chan.U.t)
        | None ->
            let chan =
              Act.Chan.create ~loc:Code_pos.dummy_loc
                (CInt.dtype ~bits:c.bitwidth)
            in
            Ir.Chan.unwrap_r chan.r)
  in

  let of_e e =
    let rec f e =
      match e with
      | Expr.Add (a, b) -> Act.CInt.E.add (f a) (f b)
      | Sub_no_wrap (a, b) -> Act.CInt.E.sub (f a) (f b)
      (* | Sub_wrap (a, b, bits) -> Act.CInt.E.sub_wrap (f a) (f b) ~bits *)
      | Mul (a, b) -> Act.CInt.E.mul (f a) (f b)
      | Div (a, b) -> Act.CInt.E.div (f a) (f b)
      | Mod (a, b) -> Act.CInt.E.mod_ (f a) (f b)
      | LShift (a, b) -> Act.CInt.E.lshift (f a) ~amt:(f b)
      | RShift (a, b) -> Act.CInt.E.rshift (f a) ~amt:(f b)
      | BitAnd (a, b) -> Act.CInt.E.bit_and (f a) (f b)
      | BitOr (a, b) -> Act.CInt.E.bit_or (f a) (f b)
      | BitXor (a, b) -> Act.CInt.E.bit_xor (f a) (f b)
      | Eq (a, b) -> Act.CInt.E.eq (f a) (f b) |> Act.CBool.E.to_int
      | Ne (a, b) -> Act.CInt.E.ne (f a) (f b) |> Act.CBool.E.to_int
      | Lt (a, b) -> Act.CInt.E.lt (f a) (f b) |> Act.CBool.E.to_int
      | Le (a, b) -> Act.CInt.E.le (f a) (f b) |> Act.CBool.E.to_int
      | Gt (a, b) -> Act.CInt.E.gt (f a) (f b) |> Act.CBool.E.to_int
      | Ge (a, b) -> Act.CInt.E.ge (f a) (f b) |> Act.CBool.E.to_int
      | Var v -> Act.CInt.E.var (of_v v)
      | Clip (e, bits) -> Act.CInt.E.clip (f e) ~bits
      | Const c -> Act.CInt.E.const c
    in
    f e
  in
  let of_e_bool e = of_e e |> Act.CBool.E.of_int |> Ir.Expr.unwrap in
  let rec of_n n =
    match n with
    | Simple_IR.Par ns -> Ir.N.Par (Code_pos.dummy_loc, List.map ns ~f:of_n)
    | Seq ns -> Seq (Code_pos.dummy_loc, List.map ns ~f:of_n)
    | DoWhile (n, guard) -> DoWhile (Code_pos.dummy_loc, of_n n, of_e_bool guard)
    | SelectImm (guard_expr, stmts) ->
        SelectImm
          ( Code_pos.dummy_loc,
            List.mapi stmts ~f:(fun i stmt ->
                let i = Act.CInt.(pow (of_int 2) (of_int i)) in
                let g =
                  Act.CInt.E.(eq (of_e guard_expr) (const i)) |> Ir.Expr.unwrap
                in
                (g, of_n stmt)),
            None )
    | Assign (id, expr) ->
        Assign
          ( Code_pos.dummy_loc,
            of_v id |> Ir.Var.untype',
            of_e expr |> Ir.Expr.untype' )
    | Send (chan, expr) ->
        Send (Code_pos.dummy_loc, of_c chan, of_e expr |> Ir.Expr.untype')
    | Read (chan, var) ->
        Read (Code_pos.dummy_loc, of_c chan, of_v var |> Ir.Var.untype')
    | Nop -> Nop
  in
  let n = of_n n in
  let user_readable_ports =
    Map.keys user_readable_ports |> List.map ~f:Ir.Chan.wrap_ru
  in
  let user_sendable_ports =
    Map.keys user_sendable_ports |> List.map ~f:Ir.Chan.wrap_wu
  in
  Sim.create (Ir.N.wrap n) ~user_readable_ports ~user_sendable_ports

let stf_sim ?(optimize = false) ir ~user_sendable_ports ~user_readable_ports =
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
  let n, my_chan_of_chan = to_simple_ir ir in
  let user_sendable_ports =
    Map.of_key_set user_sendable_ports ~f:(fun chan ->
        Hashtbl.find_exn my_chan_of_chan chan)
  in
  let user_readable_ports =
    Map.of_key_set user_readable_ports ~f:(fun chan ->
        Hashtbl.find_exn my_chan_of_chan chan)
  in
  (* print_s [%sexp (n : Simple_IR.t)]; *)
  let n = to_stf n in
  let n = if optimize then optimize_stf n else n in 
  (* print_s [%sexp (n : STF.t)]; *)
  (* print_s [%sexp (n : STF.t)]; *)

  let var_of_var = STF_Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add var_of_var v ~default:(fun () ->
        Act.Var.create (CInt.dtype ~bits:v.bitwidth))
  in

  let chan_of_my_chan = Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add chan_of_my_chan c ~default:(fun () ->
        match
          Hashtbl.to_alist my_chan_of_chan
          |> List.find ~f:(fun (_, my_chan) -> Chan.equal my_chan c)
          |> Option.bind ~f:(fun (ir_chan, _) ->
                 if
                   Map.mem user_readable_ports ir_chan
                   || Map.mem user_sendable_ports ir_chan
                 then (* TODO check bitwidths *)
                   Some ir_chan
                 else None)
        with
        | Some ir_chan -> (ir_chan : Ir.Chan.U.t)
        | None ->
            let chan =
              Act.Chan.create ~loc:Code_pos.dummy_loc
                (CInt.dtype ~bits:c.bitwidth)
            in
            Ir.Chan.unwrap_r chan.r)
  in

  let of_e e =
    let rec f e =
      match e with
      | Expr.Add (a, b) -> Act.CInt.E.add (f a) (f b)
      | Sub_no_wrap (a, b) -> Act.CInt.E.sub (f a) (f b)
      (* | Sub_wrap (a, b, bits) -> Act.CInt.E.sub_wrap (f a) (f b) ~bits *)
      | Mul (a, b) -> Act.CInt.E.mul (f a) (f b)
      | Div (a, b) -> Act.CInt.E.div (f a) (f b)
      | Mod (a, b) -> Act.CInt.E.mod_ (f a) (f b)
      | LShift (a, b) -> Act.CInt.E.lshift (f a) ~amt:(f b)
      | RShift (a, b) -> Act.CInt.E.rshift (f a) ~amt:(f b)
      | BitAnd (a, b) -> Act.CInt.E.bit_and (f a) (f b)
      | BitOr (a, b) -> Act.CInt.E.bit_or (f a) (f b)
      | BitXor (a, b) -> Act.CInt.E.bit_xor (f a) (f b)
      | Eq (a, b) -> Act.CInt.E.eq (f a) (f b) |> Act.CBool.E.to_int
      | Ne (a, b) -> Act.CInt.E.ne (f a) (f b) |> Act.CBool.E.to_int
      | Lt (a, b) -> Act.CInt.E.lt (f a) (f b) |> Act.CBool.E.to_int
      | Le (a, b) -> Act.CInt.E.le (f a) (f b) |> Act.CBool.E.to_int
      | Gt (a, b) -> Act.CInt.E.gt (f a) (f b) |> Act.CBool.E.to_int
      | Ge (a, b) -> Act.CInt.E.ge (f a) (f b) |> Act.CBool.E.to_int
      | Var v -> Act.CInt.E.var (of_v v)
      | Clip (e, bits) -> Act.CInt.E.clip (f e) ~bits
      | Const c -> Act.CInt.E.const c
    in
    f e
  in
  let rec of_n n =
    match n with
    | STF.Nop -> Ir.N.Nop
    | Assign (id, expr) ->
        Assign
          ( Code_pos.dummy_loc,
            of_v id |> Ir.Var.untype',
            of_e expr |> Ir.Expr.untype' )
    | Send (chan, expr) ->
        Send (Code_pos.dummy_loc, of_c chan, of_e expr |> Ir.Expr.untype')
    | Read (chan, var) ->
        Read (Code_pos.dummy_loc, of_c chan, of_v var |> Ir.Var.untype')
    | Seq ns -> Seq (Code_pos.dummy_loc, List.map ns ~f:of_n)
    | Par (splits, ns, merges) ->
        let prolog =
          List.concat_map splits ~f:(fun split ->
              List.filter_map split.out_vs
                ~f:
                  (Option.map ~f:(fun out_v ->
                       Ir.N.Assign
                         ( Code_pos.dummy_loc,
                           of_v out_v |> Ir.Var.untype',
                           of_e (Var split.in_v) |> Ir.Expr.untype' ))))
        in
        let postlog =
          List.concat_map merges ~f:(fun merge ->
              List.filter_map merge.in_vs
                ~f:
                  (Option.map ~f:(fun in_v ->
                       Ir.N.Assign
                         ( Code_pos.dummy_loc,
                           of_v merge.out_v |> Ir.Var.untype',
                           of_e (Var in_v) |> Ir.Expr.untype' ))))
        in
        let par = Ir.N.Par (Code_pos.dummy_loc, List.map ns ~f:of_n) in
        Seq (Code_pos.dummy_loc, List.concat [ prolog; [ par ]; postlog ])
    | SelectImm (gaurd, splits, branches, merges) ->
        let prolog =
          List.concat_map splits ~f:(fun split ->
              List.filter_map split.out_vs
                ~f:
                  (Option.map ~f:(fun out_v ->
                       Ir.N.Assign
                         ( Code_pos.dummy_loc,
                           of_v out_v |> Ir.Var.untype',
                           of_e (Var split.in_v) |> Ir.Expr.untype' ))))
        in
        let branches =
          List.mapi branches ~f:(fun i stmt ->
              let postlog =
                List.map merges ~f:(fun merge ->
                    let in_v = List.nth_exn merge.in_vs i in
                    Ir.N.Assign
                      ( Code_pos.dummy_loc,
                        of_v merge.out_v |> Ir.Var.untype',
                        of_e (Var in_v) |> Ir.Expr.untype' ))
              in

              let i = Act.CInt.(pow (of_int 2) (of_int i)) in
              let g =
                Act.CInt.E.(eq (of_e gaurd) (const i)) |> Ir.Expr.unwrap
              in
              let stmt =
                Ir.N.Seq
                  (Code_pos.dummy_loc, List.concat [ [ of_n stmt ]; postlog ])
              in
              (g, stmt))
        in
        let select = Ir.N.SelectImm (Code_pos.dummy_loc, branches, None) in
        Seq (Code_pos.dummy_loc, List.concat [ prolog; [ select ] ])
    | DoWhile (phis, stmt, guard) ->
        let guard_var = Act.Var.create (CInt.dtype ~bits:1) in

        let prolog =
          List.filter_map phis ~f:(fun phi ->
              Option.map phi.init_v ~f:(fun init_v ->
                  Ir.N.Assign
                    ( Code_pos.dummy_loc,
                      Option.value_exn phi.body_in_v |> of_v |> Ir.Var.untype',
                      of_e (Var init_v) |> Ir.Expr.untype' )))
        in
        let eval_guard =
          Ir.N.Assign
            ( Code_pos.dummy_loc,
              Ir.Var.untype' guard_var,
              of_e guard |> Ir.Expr.untype' )
        in

        let body_postlog =
          List.concat_map phis ~f:(fun phi ->
              [
                Option.map2 phi.body_in_v phi.body_out_v
                  ~f:(fun body_in_v body_out_v ->
                    Ir.N.Assign
                      ( Code_pos.dummy_loc,
                        of_v body_in_v |> Ir.Var.untype',
                        of_e (Var body_out_v) |> Ir.Expr.untype' ));
                Option.map phi.out_v ~f:(fun out_v ->
                    Ir.N.Assign
                      ( Code_pos.dummy_loc,
                        of_v out_v |> Ir.Var.untype',
                        of_e (Var (Option.value_exn phi.body_out_v))
                        |> Ir.Expr.untype' ));
              ])
          |> List.filter_opt
        in
        let body =
          Ir.N.Seq
            ( Code_pos.dummy_loc,
              List.concat [ [ of_n stmt; eval_guard ]; body_postlog ] )
        in
        let loop =
          Ir.N.DoWhile
            ( Code_pos.dummy_loc,
              body,
              CInt.E.var guard_var |> CBool.E.of_int |> Ir.Expr.unwrap )
        in
        Seq (Code_pos.dummy_loc, List.concat [ prolog; [ loop ] ])
  in
  let n = of_n n in
  (* print_s [%sexp (n : Ir.N.t)]; *)
  let user_readable_ports =
    Map.keys user_readable_ports |> List.map ~f:Ir.Chan.wrap_ru
  in
  let user_sendable_ports =
    Map.keys user_sendable_ports |> List.map ~f:Ir.Chan.wrap_wu
  in
  Sim.create (Ir.N.wrap n) ~user_readable_ports ~user_sendable_ports
