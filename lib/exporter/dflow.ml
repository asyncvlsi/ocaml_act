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

module Dflow_id_set = struct
  module T = struct
    type t = Dflow_id.Set.t [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Stmt = struct
  module Guard = struct
    module T = struct
      type t =
        | One_hot of Dflow_id.t
        | Idx of Dflow_id.t
        | Bits of Dflow_id.t list
      [@@deriving sexp, compare, equal]
    end

    include T
    include Comparable.Make (T)

    let ids g = match g with One_hot g | Idx g -> [ g ] | Bits gs -> gs

    let map g ~f =
      match g with
      | One_hot g -> One_hot (f g)
      | Idx g -> Idx (f g)
      | Bits g -> Bits (List.map ~f g)
  end

  type t =
    | MultiAssign of (Dflow_id.t * Dflow_id.t Expr.t) list
    | Split of Guard.t * Dflow_id.t * Dflow_id.t option list
    | Merge of Guard.t * Dflow_id.t list * Dflow_id.t
    | Copy_init of (*dst *) Dflow_id.t * (*src*) Dflow_id.t * Act.CInt.t
  [@@deriving sexp_of]
end

include Stmt

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

let merge_bool_guard s (b1, b2) v = [ Merge (Idx s, [ b1; b2 ], v) ]
let split_bool_guard s v (b1, b2) = [ Split (Idx s, v, [ b1; b2 ]) ]
let assign dst e = MultiAssign [ (dst, e) ]

(* control processes from rui's paper (converted into dataflow and adapted a bit). *)
let rui_ctrl_chan ctrl ~new_chan ~dir:_ =
  let s_ = new_chan 1 in
  [
    Copy_init (ctrl, s_, CInt.zero);
    assign s_ (BitXor (Const CInt.one, Var ctrl));
  ]

let rui_ctrl_send = rui_ctrl_chan ~dir:`Send
let rui_ctrl_read = rui_ctrl_chan ~dir:`Read

let rui_ctrl_seq a b_chan a1 b1 a2 b2 ~new_chan ~dir =
  let x_width = a.Dflow_id.bitwidth in
  let s = new_chan 1 in
  (* [ ~s -> b1?v [] s -> b2?v ] *)
  let v = new_chan 1 in
  let dflow1 = merge_bool_guard s (b1, b2) v in
  (* [ ~ (s | v) -> skip  []   (s | v) -> b!v   ] *)
  let dflow2 =
    let g = new_chan 1 in
    assign g (Expr.BitOr (Var s, Var v))
    :: split_bool_guard g v (None, Some b_chan)
  in

  (* EITHER [ ~v -> s := ~s  []   v -> a?x; [ ~s -> a1!x  []   s -> a2!x ] ]
     OR  [    ~v -> s := ~s  []   v -> [ ~s -> a1?x  []   s -> a2?x ] a!x; ] *)
  let s' = new_chan x_width in
  let dflow3 =
    let s0 = new_chan 1 in
    let s1 = new_chan 1 in
    let s0' = new_chan 1 in
    let op_procs =
      match dir with
      | `Read -> split_bool_guard s1 a (Some a1, Some a2)
      | `Send -> merge_bool_guard s1 (a1, a2) a
    in
    [
      split_bool_guard v s (Some s0, Some s1);
      [ assign s0' (Expr.BitXor (Var s0, Const CInt.one)) ];
      merge_bool_guard v (s0', s1) s';
      op_procs;
    ]
    |> List.concat
  in
  (* finally, we must wrap `s` back around to the top *)
  let dflow_wrap = [ Copy_init (s, s', CInt.zero) ] in
  dflow1 @ dflow2 @ dflow3 @ dflow_wrap

let rui_ctrl_seq_send = rui_ctrl_seq ~dir:`Send
let rui_ctrl_seq_read = rui_ctrl_seq ~dir:`Read

let rui_ctrl_select a_chan b_chan guards ab_list ~new_chan ~dir =
  let g_width = List.length guards in
  let v = new_chan 1 in
  let g = new_chan g_width in

  (* [ ~v -> c?g [] v -> skip ] *)
  let g' = new_chan g_width in
  let dflow1 =
    let g1 = new_chan g_width in
    let c = new_chan g_width in
    let c_expr = Expr.Concat (List.map guards ~f:(fun e -> (Expr.Var e, 1))) in
    [
      split_bool_guard v g (None, Some g1);
      (* pack guards into c. TODO get around this somehow to expose this to the optimizer better. *)
      [ assign c c_expr ];
      merge_bool_guard v (c, g1) g';
    ]
    |> List.concat
  in

  (* [ g{i}=1   ->   B_i?v ]; B!v; *)
  let v' = new_chan 1 in
  let dflow2 =
    let dflows, vi's =
      List.map ab_list ~f:(fun o ->
          let vi' = new_chan 1 in
          let dflow =
            match o with
            | None -> assign vi' (Const CInt.zero)
            | Some (_, bi) -> assign vi' (Var bi)
          in
          (dflow, vi'))
      |> List.unzip
    in
    List.concat
      [ [ Merge (One_hot g', vi's, v'); assign b_chan (Var v') ]; dflows ]
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
                  | None -> assign vi' (Const CInt.zero)
                  | Some (ai, _) -> assign vi' (Var ai)
                in
                (dflow, vi'))
            |> List.unzip
          in
          Merge (One_hot g1', ais, a_chan) :: dflows
      | `Read ->
          let ais = List.map ab_list ~f:(Option.map ~f:fst) in
          [ Split (One_hot g1', a_chan, ais) ]
    in
    split_bool_guard v' g' (None, Some g1') @ op_procs
  in

  (* finally, we must wrap `v` and `g` back around to the top *)
  let dflow_wrap =
    [ Copy_init (v, v', CInt.zero); Copy_init (g, g', CInt.one) ]
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
      [ assign v (Var b1) ];
      split_bool_guard v g (None, Some g1);
      merge_bool_guard v (guard_chan, g1) g';
    ]
    |> List.concat
  in
  (* [ ~(v | ~g)  -> skip  []  (v | ~g) -> B!b ]; *)
  let dflow2 =
    let e = new_chan 1 in

    assign e (BitOr (Var v, Eq0 (Var g)))
    :: split_bool_guard e v (None, Some b_chan)
  in

  (* finally, we must wrap `g` back around to the top *)
  let dflow_wrap = [ Copy_init (g, g', CInt.zero) ] in
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

  let of_e e = Expr.map_vars e ~f:(fun v -> of_v v) in
  let of_e' (e : Stf.Var.t Expr.t) =
    let bitwidth = Expr.bitwidth e ~bits_of_var:(fun v -> v.bitwidth) in
    let v = new_chan bitwidth in
    (assign v (of_e e), v)
  in

  (* returns a tuple (dflow, alias_map) where alias_map is a map { Chan_end.t -> (alias, ctrl)} *)
  let rec of_stmt n =
    match n with
    | Stf.Stmt.Nop -> ([], Chan_end.Map.empty)
    | Assign (v, e) -> ([ assign (of_v v) (of_e e) ], Chan_end.Map.empty)
    | Send (c, e) ->
        let cc = new_chan c.bitwidth in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Send c) (cc, ctrl) in
        let dflow = [ assign cc (of_e e) ] in
        let ctrl_procs = rui_ctrl_send ctrl ~new_chan in
        let dflow = List.concat [ dflow; ctrl_procs ] in
        (dflow, alias_map)
    | Read (c, v) ->
        let cc = new_chan c.bitwidth in
        let ctrl = new_ctrl () in
        let alias_map = Chan_end.Map.singleton (Chan_end.Read c) (cc, ctrl) in
        let dflow = [ assign (of_v v) (Var cc) ] in
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
                     assign (of_v out_v) (Var (of_v split.in_v))))
        in
        let merges =
          List.concat_map merges ~f:(fun merge ->
              List.filter_map merge.in_vs ~f:Fn.id
              |> List.map ~f:(fun in_v ->
                     assign (of_v merge.out_v) (Var (of_v in_v))))
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
    | SelectImm (guards, splits, branches, merges) ->
        let guard_procs, guards = List.map ~f:of_e' guards |> List.unzip in
        let splits =
          List.map splits ~f:(fun split ->
              let out_vs = List.map split.out_vs ~f:(Option.map ~f:of_v) in
              Split (Bits guards, of_v split.in_v, out_vs))
        in
        let merges =
          List.map merges ~f:(fun merge ->
              let in_vs = List.map merge.in_vs ~f:of_v in
              Merge (Bits guards, in_vs, of_v merge.out_v))
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
                      rui_ctrl_select_send a_chan b_chan guards aliases
                        ~new_chan
                  | Read _ ->
                      rui_ctrl_select_read a_chan b_chan guards aliases
                        ~new_chan
                in
                ((k, (a_chan, b_chan)), ctrl_procs))
          in
          let alias_list, ctrl_procs = List.unzip l in
          (Chan_end.Map.of_alist_exn alias_list, List.concat ctrl_procs)
        in
        let branches = List.concat_map branches ~f:fst in
        let dflow =
          List.concat [ guard_procs; splits; merges; branches; ctrl_procs ]
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
                    Merge (Idx prev_guard, [ init_v; carry ], of_v body_in_v))
              in
              let bottom =
                Option.map phi.body_out_v ~f:(fun body_out_v ->
                    let out_vs = [ Option.map phi.out_v ~f:of_v; Some carry ] in
                    Split (Idx guard, of_v body_out_v, out_vs))
              in
              let copy_init = Copy_init (prev_guard, guard, CInt.zero) in
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

let var_ids dflows iports oports =
  [
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign assigns ->
            List.concat_map assigns ~f:(fun (dst, e) -> dst :: Expr.var_ids e)
        | Split (g, v, os) -> (v :: Guard.ids g) @ List.filter_opt os
        | Merge (g, ins, v) -> (v :: Guard.ids g) @ ins
        | Copy_init (dst, src, _) -> [ dst; src ]);
    List.map iports ~f:snd;
    List.map oports ~f:snd;
  ]
  |> List.concat |> Dflow_id.Set.of_list

let optimize_proc proc =
  let eliminate_dead_code proc =
    let { Proc.stmt = dflows; iports; oports } = proc in
    let dependecies_of_id =
      List.concat_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign assigns ->
              List.concat_map assigns ~f:(fun (dst, e) ->
                  Expr.var_ids e |> List.map ~f:(fun src_id -> (dst, src_id)))
          | Split (g, v, os) ->
              List.filter_opt os
              |> List.concat_map ~f:(fun o ->
                     (o, v) :: List.map (Guard.ids g) ~f:(fun g -> (o, g)))
          | Merge (g, ins, v) ->
              List.map (Guard.ids g @ ins) ~f:(fun i -> (v, i))
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
    let dflows =
      List.filter_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign assigns ->
              if List.exists assigns ~f:(fun (dst, _) -> Set.mem alive dst) then
                Some (MultiAssign assigns)
              else None
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
          | Copy_init (dst, src, init) ->
              if Set.mem alive dst then Some (Copy_init (dst, src, init))
              else None)
    in
    { Proc.stmt = dflows; oports; iports }
  in

  let eliminate_repeated_vars proc =
    let { Proc.stmt = dflows; iports; oports } = proc in
    let src_of_dst =
      List.filter_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign [ (dst, Var src) ] -> Some (dst, src)
          | _ -> None)
      |> Dflow_id.Map.of_alist_exn
    in

    (* This is probably way slower than needed *)
    let true_src ~dst ~src =
      let rec h x =
        if Dflow_id.equal x dst then None
        else
          let pred = Map.find src_of_dst x in
          match pred with None -> Some x | Some pred -> h pred
      in
      h src
    in
    let true_src_of_dst =
      Map.filter_mapi src_of_dst ~f:(fun ~key ~data ->
          true_src ~dst:key ~src:data)
    in

    let of_v v = Map.find true_src_of_dst v |> Option.value ~default:v in
    let of_vo o = Option.map o ~f:of_v in

    let dflows =
      List.map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign assigns ->
              let assigns =
                List.map assigns ~f:(fun (dst, e) ->
                    (dst, Expr.map_vars e ~f:of_v))
              in
              MultiAssign assigns
          | Split (g, v, os) ->
              Split (Guard.map ~f:of_v g, of_v v, List.map os ~f:of_vo)
          | Merge (g, ins, v) ->
              Merge (Guard.map ~f:of_v g, List.map ~f:of_v ins, of_v v)
          | Copy_init (dst, src, init) -> Copy_init (of_v dst, of_v src, init))
    in
    let iports = List.map iports ~f:(fun (i, v) -> (i, of_v v)) in
    let oports = List.map oports ~f:(fun (i, v) -> (i, of_v v)) in
    { Proc.stmt = dflows; oports; iports }
  in

  (* A compilcation when optimizing dataflow is that one cannot, in general, safely ADD OR REMOVE reads for a node.
     In particular, this means that optimizing expressions is hard, since an expression still needs to read a token
     for timing reasons even if it never looks at it. Moreover, fusing two assigns into a block is in general not
     allowed, because the MultiAssign block will only fire one it has received all the necassary tokens.

     However, the following three transformations are always valid.
     1) If two nodes read the same set of tokens, they may be merged together.
     2) If one node is the only node that reads the output of a second node, the second node may be merged into the first.
     3) We may duplicate a node, and may assign some readers of the original node to read the duplicate node instead.

     Here I implement the first and second optimization. More general breaking/clusering work could be useful *)

  (* Transofmation #1 *)
  let cluster_same_reads { Proc.stmt = dflows; iports; oports } =
    let multi_assigns, non_assigns =
      List.partition_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign multi_assign -> First multi_assign
          | _ -> Second dflow)
    in

    let multi_assigns =
      List.map multi_assigns ~f:(fun multi_assign ->
          let reads =
            List.concat_map multi_assign ~f:(fun (_, e) -> Expr.var_ids e)
            |> Dflow_id.Set.of_list
          in
          (reads, multi_assign))
      |> Dflow_id_set.Map.of_alist_multi |> Map.to_alist
      |> List.concat_map ~f:(fun (reads, multi_assigns) ->
             if Set.is_empty reads then
               List.map multi_assigns ~f:(fun multi_assign ->
                   MultiAssign multi_assign)
             else [ MultiAssign (List.concat multi_assigns) ])
    in

    { Proc.stmt = multi_assigns @ non_assigns; oports; iports }
  in

  (* Transofmation #2 *)
  let cluster_fuse_chains { Proc.stmt = dflows; iports; oports } =
    let dflows = List.mapi dflows ~f:(fun id dflow -> (id, dflow)) in
    let multi_assigns, non_assigns =
      List.partition_map dflows ~f:(fun (id, dflow) ->
          match dflow with
          | MultiAssign multi_assign -> First (id, multi_assign)
          | _ -> Second dflow)
    in

    let cluster_of_output =
      List.concat_map multi_assigns ~f:(fun (cluster_id, multi_assign) ->
          List.map multi_assign ~f:fst
          |> Dflow_id.Set.of_list |> Set.to_list
          |> List.map ~f:(fun dflow_id -> (dflow_id, cluster_id)))
      |> Dflow_id.Map.of_alist_exn
    in
    (* let read_cluster_of_id = Map.map multi_assigns ~f:(fun multi_assign -> List.concat_map multi_assign ~f:(fun (_, e) -> Expr.var_ids)
       |> List.filter_map ~f:(Map.find id_of_output)
       |> Int.Set.of_list) in
    *)
    let read_ct_of_cluster =
      List.map dflows ~f:(fun (_, dflow) ->
          (match dflow with
          | MultiAssign assigns ->
              List.concat_map assigns ~f:(fun (_, e) -> Expr.var_ids e)
          | Split (g, v, os) -> (v :: Guard.ids g) @ List.filter_opt os
          | Merge (g, ins, v) -> (v :: Guard.ids g) @ ins
          | Copy_init (dst, src, _) -> [ dst; src ])
          |> List.filter_map ~f:(Map.find cluster_of_output)
          |> Int.Set.of_list)
      |> List.concat_map ~f:Set.to_list
      |> List.map ~f:(fun v -> (v, ()))
      |> Int.Map.of_alist_multi |> Map.map ~f:List.length
    in

    let consumer_of_producer =
      List.concat_map multi_assigns ~f:(fun (id_i, _) ->
          List.filter_map multi_assigns ~f:(fun (id_o, multi_assign) ->
              let reads_of_id_o =
                List.concat_map multi_assign ~f:(fun (_, e) -> Expr.var_ids e)
                |> List.filter_map ~f:(Map.find cluster_of_output)
                |> Int.Set.of_list
              in
              if
                Int.equal 1
                  (Map.find read_ct_of_cluster id_i |> Option.value ~default:0)
                && Set.mem reads_of_id_o id_i
              then Some (id_i, id_o)
              else None))
      |> Int.Map.of_alist_exn
    in

    let true_consumer ~producer ~consumer =
      let rec h x =
        if Int.equal x producer then None
        else
          let pred = Map.find consumer_of_producer x in
          match pred with None -> Some x | Some pred -> h pred
      in
      h consumer
    in
    let true_consumer_of_producer =
      Map.filter_mapi consumer_of_producer ~f:(fun ~key ~data ->
          true_consumer ~producer:key ~consumer:data)
    in
    let multi_assigns =
      List.map multi_assigns ~f:(fun (cluster_id, multi_assign) ->
          let cluster_id =
            Map.find true_consumer_of_producer cluster_id
            |> Option.value ~default:cluster_id
          in
          (cluster_id, multi_assign))
      |> Int.Map.of_alist_multi |> Map.map ~f:List.concat |> Map.data
      |> List.map ~f:(fun multi_assign -> MultiAssign multi_assign)
    in
    { Proc.stmt = multi_assigns @ non_assigns; oports; iports }
  in
  let normalize_guards (proc : Proc.t) =
    let next_id =
      let biggest_id =
        var_ids proc.stmt proc.iports proc.oports
        |> Set.to_list
        |> List.map ~f:(fun v -> v.id)
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
      in
      ref (biggest_id + 1)
    in
    let new_chan bitwidth =
      let id = !next_id in
      incr next_id;
      { Dflow_id.id; bitwidth }
    in
    let dflows = proc.stmt in
    let guards =
      List.filter_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign _ -> None
          | Copy_init _ -> None
          | Merge (g, _, _) -> Some g
          | Split (g, _, _) -> Some g)
      |> Guard.Set.of_list
      |> Guard.Map.of_key_set ~f:(fun g ->
             (* let log2_bits bits =

                  List.mapi bits ~f:(fun i bit ->
                      Expr.(Mul (Const (CInt.of_int i), bit)))
                  |> List.reduce ~f:(fun a b -> Expr.BitOr (a, b))
                  |> Option.value ~default:(Expr.Const CInt.zero)
                in *)
             match g with
             | Idx g -> (Guard.Idx g, None)
             | One_hot g ->
                 let new_g = new_chan (Int.ceil_log2 g.bitwidth) in
                 let expr =
                   Expr.Log2OneHot (Var g)
                 in
                 let assign = assign new_g expr in
                 (Idx new_g, Some assign)
             | Bits gs ->
                 let bits = Int.ceil_log2 (List.length gs) in
                 let new_g = new_chan bits in
                 let gs = List.map gs ~f:(fun g -> Expr.Var g, 1) in
                 let assign = assign new_g (Log2OneHot (Concat gs)) in
                 (Idx new_g, Some assign))
    in
    let procs = Map.data guards |> List.filter_map ~f:snd in
    let guards = Map.map guards ~f:fst in
    let dflows =
      List.map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign assigns -> MultiAssign assigns
          | Copy_init (dst, src, v) -> Copy_init (dst, src, v)
          | Merge (g, srcs, dst) -> Merge (Map.find_exn guards g, srcs, dst)
          | Split (g, src, dsts) -> Split (Map.find_exn guards g, src, dsts))
    in
    let dflows = dflows @ procs in
    { Proc.stmt = dflows; iports = proc.iports; oports = proc.oports }
  in

  eliminate_dead_code proc |> eliminate_repeated_vars |> eliminate_dead_code
  |> cluster_same_reads |> cluster_fuse_chains |> cluster_same_reads
  |> eliminate_repeated_vars |> eliminate_dead_code |> normalize_guards
  |> cluster_same_reads |> eliminate_repeated_vars |> eliminate_dead_code
