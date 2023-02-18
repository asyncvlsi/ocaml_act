open! Core
module CInt = Act.CInt

module Dflow_id = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Stmt = struct
  type t =
    | Assign of Dflow_id.t * Dflow_id.t Expr.t
    | Split of Dflow_id.t * Dflow_id.t * Dflow_id.t option list
    | Merge of Dflow_id.t * Dflow_id.t list * Dflow_id.t
    | MergeBoolGuard of Dflow_id.t * (Dflow_id.t * Dflow_id.t) * Dflow_id.t
    | SplitBoolGuard of
        Dflow_id.t * Dflow_id.t * (Dflow_id.t option * Dflow_id.t option)
    | Copy_init of Dflow_id.t * Dflow_id.t * CInt.t
  [@@deriving sexp_of]
end

module Proc = struct
  type t = {
    stmt : Stmt.t list;
    iports : (Interproc_chan.t * Dflow_id.t) list;
    oports : (Interproc_chan.t * Dflow_id.t) list;
  }
  [@@deriving sexp_of]
end

module Chan_end = struct
  module T = struct
    type t = Read of Stf.Chan.t | Send of Stf.Chan.t
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
    Stmt.Copy_init (ctrl, s_, CInt.zero);
    Assign (s_, BitXor (Const CInt.one, Var ctrl));
  ]

let rui_ctrl_send = rui_ctrl_chan ~dir:`Send
let rui_ctrl_read = rui_ctrl_chan ~dir:`Read

let rui_ctrl_seq a b_chan a1 b1 a2 b2 ~new_chan ~dir =
  let x_width = a.Dflow_id.bitwidth in
  let s = new_chan 1 in
  (* [ ~s -> b1?v [] s -> b2?v ] *)
  let v = new_chan 1 in
  let dflow1 = [ Stmt.MergeBoolGuard (s, (b1, b2), v) ] in
  (* [ ~ (s | v) -> skip  []   (s | v) -> b!v   ] *)
  let dflow2 =
    let g = new_chan 1 in
    [
      Stmt.Assign (g, Expr.BitOr (Var s, Var v));
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
      | `Read -> Stmt.SplitBoolGuard (s1, a, (Some a1, Some a2))
      | `Send -> MergeBoolGuard (s1, (a1, a2), a)
    in
    [
      Stmt.SplitBoolGuard (v, s, (Some s0, Some s1));
      Assign (s0', Expr.BitXor (Var s0, Const CInt.one));
      MergeBoolGuard (v, (s0', s1), s');
      op_proc;
    ]
  in
  (* finally, we must wrap `s` back around to the top *)
  let dflow_wrap = [ Stmt.Copy_init (s, s', CInt.zero) ] in
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
      Stmt.SplitBoolGuard (v, g, (None, Some g1));
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
            | None -> Stmt.Assign (vi', Const CInt.zero)
            | Some (_, bi) -> Stmt.Assign (vi', Var bi)
          in
          (dflow, vi'))
      |> List.unzip
    in
    List.concat
      [ [ Stmt.Merge (g', vi's, v'); Assign (b_chan, Var v') ]; dflows ]
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
                  | None -> Stmt.Assign (vi', Const CInt.zero)
                  | Some (ai, _) -> Stmt.Assign (vi', Var ai)
                in
                (dflow, vi'))
            |> List.unzip
          in
          Stmt.Merge (g1', ais, a_chan) :: dflows
      | `Read ->
          let ais = List.map ab_list ~f:(Option.map ~f:fst) in
          [ Split (g1', a_chan, ais) ]
    in
    Stmt.SplitBoolGuard (v', g', (None, Some g1')) :: op_procs
  in

  (* finally, we must wrap `v` and `g` back around to the top *)
  let dflow_wrap =
    [ Stmt.Copy_init (v, v', CInt.zero); Copy_init (g, g', CInt.one) ]
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
      Stmt.Assign (v, Var b1);
      SplitBoolGuard (v, g, (None, Some g1));
      MergeBoolGuard (v, (guard_chan, g1), g');
    ]
  in
  (* [ ~(v | ~g)  -> skip  []  (v | ~g) -> B!b ]; *)
  let dflow2 =
    let e = new_chan 1 in
    [
      Stmt.Assign (e, BitOr (Var v, BitXor (Var g, Const CInt.one)));
      SplitBoolGuard (e, v, (None, Some b_chan));
    ]
  in
  (* finally, we must wrap `g` back around to the top *)
  let dflow_wrap = [ Stmt.Copy_init (g, g', CInt.zero) ] in
  dflow1 @ dflow2 @ dflow_wrap

let rui_ctrl_dowhile_send = rui_ctrl_dowhile ~dir:`Send
let rui_ctrl_dowhile_read = rui_ctrl_dowhile ~dir:`Send

let dflow_of_stf proc =
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Dflow_id.id; bitwidth }
  in
  let new_alias (a : Dflow_id.t) = new_chan a.bitwidth in
  let new_ctrl () = new_chan 1 in

  let id_of_var = Stf.Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add id_of_var v ~default:(fun () -> new_chan v.bitwidth)
  in

  let id_of_chan = Stf.Chan.Table.create () in
  let of_c c =
    Hashtbl.find_or_add id_of_chan c ~default:(fun () -> new_chan c.bitwidth)
  in

  let of_e e = Expr.map_vars e ~f:(fun v -> of_v v) in
  let of_e' (e : Stf.Var.t Expr.t) =
    let bitwidth = Expr.bitwidth e ~bits_of_var:(fun v -> v.bitwidth) in
    let v = new_chan bitwidth in
    (Stmt.Assign (v, of_e e), v)
  in

  (* returns a tuple (dflow, alias_map) where alias_map is a map { Chan_end.t -> (alias, ctrl)} *)
  let rec of_stmt n =
    match n with
    | Stf.Stmt.Nop -> ([], Chan_end.Map.empty)
    | Assign (v, e) -> ([ Stmt.Assign (of_v v, of_e e) ], Chan_end.Map.empty)
    | Send (c, e) ->
        let cc = of_c c in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Send c) (cc, ctrl) in
        let dflow = [ Stmt.Assign (cc, of_e e) ] in
        let ctrl_procs = rui_ctrl_send ctrl ~new_chan in
        let dflow = List.concat [ dflow; ctrl_procs ] in
        (dflow, alias_map)
    | Read (c, v) ->
        let cc = of_c c in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Read c) (cc, ctrl) in
        let dflow = [ Stmt.Assign (of_v v, Var (of_c c)) ] in
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
                     Stmt.Assign (of_v out_v, Var (of_v split.in_v))))
        in
        let merges =
          List.concat_map merges ~f:(fun merge ->
              List.filter_map merge.in_vs ~f:Fn.id
              |> List.map ~f:(fun in_v ->
                     Stmt.Assign (of_v merge.out_v, Var (of_v in_v))))
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
              Stmt.Split (guard_expr, of_v split.in_v, out_vs))
        in
        let merges =
          List.map merges ~f:(fun merge ->
              let in_vs = List.map merge.in_vs ~f:of_v in
              Stmt.Merge (guard_expr, in_vs, of_v merge.out_v))
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
                    Stmt.Merge (prev_guard, [ init_v; carry ], of_v body_in_v))
              in
              let bottom =
                Option.map phi.body_out_v ~f:(fun body_out_v ->
                    let out_vs = [ Option.map phi.out_v ~f:of_v; Some carry ] in
                    Stmt.Split (guard, of_v body_out_v, out_vs))
              in
              let copy_init = Stmt.Copy_init (prev_guard, guard, CInt.zero) in
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
  let dflow, alias_map = of_stmt proc.Stf.Proc.stmt in
  let iports =
    List.map proc.iports ~f:(fun (interproc, chan) ->
        let chan, _ = Map.find_exn alias_map (Read chan) in
        (interproc, chan))
  in
  let oports =
    List.map proc.oports ~f:(fun (interproc, chan) ->
        let chan, _ = Map.find_exn alias_map (Send chan) in
        (interproc, chan))
  in
  { Proc.stmt = dflow; iports; oports }

let optimize_proc proc =
  let eliminate_dead_code dflows ~oports =
    let dependecies_of_id =
      List.concat_map dflows ~f:(fun dflow ->
          match dflow with
          | Stmt.Assign (dst, e) ->
              Expr.var_ids e |> List.map ~f:(fun src_id -> (dst, src_id))
          | Split (g, v, os) ->
              List.filter_opt os
              |> List.concat_map ~f:(fun o -> [ (o, g); (o, v) ])
          | Merge (g, ins, v) -> List.map (g :: ins) ~f:(fun i -> (v, i))
          | MergeBoolGuard (g, (i1, i2), v) ->
              List.map [ g; i1; i2 ] ~f:(fun i -> (v, i))
          | SplitBoolGuard (g, v, (o1, o2)) ->
              List.filter_opt [ o1; o2 ]
              |> List.concat_map ~f:(fun o -> [ (o, g); (o, v) ])
          | Copy_init (dst, src, _) -> [ (dst, src) ])
      |> Dflow_id.Map.of_alist_multi
    in
    let alive = Dflow_id.Table.create () in
    let queue = Queue.create () in
    List.iter oports ~f:(fun (_, port) ->
        Hashtbl.find_or_add alive port ~default:(fun () ->
            Queue.enqueue queue port));
    while Queue.length queue > 0 do
      let id = Queue.dequeue_exn queue in
      let deps = Map.find dependecies_of_id id |> Option.value ~default:[] in
      List.iter deps ~f:(fun id ->
          Hashtbl.find_or_add alive id ~default:(fun () ->
              Queue.enqueue queue id))
    done;
    let alive = Hashtbl.keys alive |> Dflow_id.Set.of_list in
    List.filter_map dflows ~f:(fun dflow ->
        match dflow with
        | Assign (dst, e) ->
            if Set.mem alive dst then Some (Stmt.Assign (dst, e)) else None
        | Split (g, v, os) ->
            let os =
              List.map os
                ~f:
                  (Option.bind ~f:(fun o ->
                       if Set.mem alive o then Some o else None))
            in
            if List.exists os ~f:Option.is_some then Some (Split (g, v, os))
            else None
        | Merge (g, ins, v) ->
            if Set.mem alive v then Some (Merge (g, ins, v)) else None
        | MergeBoolGuard (g, (i1, i2), v) ->
            if Set.mem alive v then Some (MergeBoolGuard (g, (i1, i2), v))
            else None
        | SplitBoolGuard (g, v, (o1, o2)) ->
            let o1 =
              Option.bind o1 ~f:(fun o ->
                  if Set.mem alive o then Some o else None)
            in
            let o2 =
              Option.bind o2 ~f:(fun o ->
                  if Set.mem alive o then Some o else None)
            in
            if Option.is_some o1 || Option.is_some o2 then
              Some (SplitBoolGuard (g, v, (o1, o2)))
            else None
        | Copy_init (dst, src, init) ->
            if Set.mem alive dst then Some (Copy_init (dst, src, init))
            else None)
  in
  let dflows = eliminate_dead_code proc.Proc.stmt ~oports:proc.oports in
  { Proc.stmt = dflows; iports = proc.iports; oports = proc.oports }
