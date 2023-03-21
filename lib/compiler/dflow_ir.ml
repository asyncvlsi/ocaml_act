open! Core
module CInt = Act.CInt

module Var = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

module Var_set = struct
  module T = struct
    type t = Var.Set.t [@@deriving compare, equal, sexp]
  end

  include T
  include Comparable.Make (T)
end

module FBlock = Fblock.Make (Var)

module Stmt = struct
  module Guard = struct
    module T = struct
      type t = One_hot of Var.t | Idx of Var.t | Bits of Var.t list
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
    | MultiAssign of FBlock.t
    | Split of Guard.t * Var.t * Var.t option list
    | Merge of Guard.t * Var.t list * Var.t
    | Buff1 of (*dst *) Var.t * (*src*) Var.t * Act.CInt.t option
  [@@deriving sexp_of]
end

include Stmt

module Proc = struct
  type t = {
    stmt : Stmt.t list;
    iports : (Interproc_chan.t * Var.t) list;
    oports : (Interproc_chan.t * Var.t) list;
  }
  [@@deriving sexp_of]
end

let validate proc =
  let { Proc.stmt = dflows; iports; oports } = proc in
  (* Check that each id is written at most one time, and that each read id is
     written at least once *)
  let reads =
    [
      List.concat_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign fblock -> FBlock.ins fblock
          | Split (g, v, _) -> v :: Guard.ids g
          | Merge (g, ins, _) -> Guard.ids g @ ins
          | Buff1 (_, src, _) -> [ src ]);
      List.map oports ~f:snd;
    ]
    |> List.concat |> Var.Set.of_list
  in

  let writes =
    [
      List.concat_map dflows ~f:(fun dflow ->
          match dflow with
          | MultiAssign fblock -> FBlock.outs fblock
          | Split (_, _, os) -> List.filter_opt os
          | Merge (_, _, v) -> [ v ]
          | Buff1 (dst, _, _) -> [ dst ]);
      List.map iports ~f:snd;
    ]
    |> List.concat
  in
  let repeated_write_cts =
    List.map writes ~f:(fun w -> (w, ()))
    |> Var.Map.of_alist_multi |> Map.map ~f:List.length
    |> Map.filter ~f:(fun ln -> ln >= 2)
    |> Map.to_alist
  in
  (match repeated_write_cts with
  | [] -> ()
  | repeated_write_cts ->
      print_s [%sexp ("error" : string)];
      print_s [%sexp (proc : Proc.t)];
      print_s [%sexp (repeated_write_cts : (Var.t * int) list)];

      failwith "Some variable is written more than once");
  let writes = Var.Set.of_list writes in
  assert (Set.diff reads writes |> Set.is_empty);

  (* Check that the bitwidths match as expected *)
  List.iter dflows ~f:(fun dflow ->
      match dflow with
      | MultiAssign _ -> ()
      | Split (_, _, _) -> ()
      | Merge (_, _, _) -> ()
      | Buff1 (dst, src, _) ->
          if not (Int.equal dst.bitwidth src.bitwidth) then (
            print_s [%sexp ((dst, src) : Var.t * Var.t)];
            assert false));

  ()

module Chan_end = struct
  module T = struct
    type t = Read of Stf.Chan.t | Send of Stf.Chan.t
    [@@deriving sexp, equal, hash, compare]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)
end

module Rui = struct
  module Ored_guard = struct
    type t = Var.t Nonempty_list.t [@@deriving sexp]
  end

  module Proc = struct
    type t =
      | Op of Var.t
      | Seq of t list
      | SelectImm of
          (* `Or`ed together *) (Ored_guard.t * t) list * Ored_guard.t option
      | DoWhile of t * Var.t
      | Loop of t
    [@@deriving sexp]
  end

  let optimize rui =
    let rec f rui =
      match rui with
      | Proc.Op v -> Some (Proc.Op v)
      | Seq ruis -> (
          let ruis = List.filter_map ruis ~f in
          let ruis =
            List.concat_map ruis ~f:(fun rui ->
                match rui with Seq ruis -> ruis | _ -> [ rui ])
          in
          match ruis with
          | [] -> None
          | [ c ] -> Some c
          | ruis -> Some (Proc.Seq ruis))
      | SelectImm (some_branches, else_guards) -> (
          let some_branches, more_none_guards =
            List.map some_branches ~f:(fun (g, rui) -> (g, f rui))
            |> List.partition_map ~f:(fun (g, rui) ->
                   match rui with
                   | Some rui -> Either.First (g, rui)
                   | None -> Either.Second g)
          in
          let else_guards =
            Nonempty_list.concat (Option.to_list else_guards @ more_none_guards)
          in
          match (some_branches, else_guards) with
          | [], _ -> None
          | [ (_, n) ], None -> Some n
          | some_branches, _ -> Some (SelectImm (some_branches, else_guards)))
      | DoWhile (rui, g) ->
          Option.map (f rui) ~f:(fun rui -> Proc.DoWhile (rui, g))
      | Loop rui -> (
          match f rui with
          | None -> None
          | Some (SelectImm ([ (_, n) ], _)) -> Some (Loop n)
          | Some rui -> Some (Loop rui))
    in
    let%bind.Option rui = f rui in
    (* a top-level do-while loop may be repleaced with a loop because each
       channel is read infinitely many times *)
    let rui =
      match rui with
      | Proc.Op v -> Proc.Op v
      | Seq ruis -> Seq ruis
      | SelectImm (some_branches, else_guards) ->
          SelectImm (some_branches, else_guards)
      | DoWhile (rui, _) -> Loop rui
      | Loop rui -> Loop rui
    in
    f rui

  module Pre_synth = struct
    (* TODO Add direct support for "Op-seq"s and for "conditionaled Op-seq"s *)
    type t =
      | Op of Var.t
      (* A sequence consisting of Op!op and [ g -> Op!op [] else -> skip ] *)
      | Opt_guarded_op_seq of
          (* guard *) (Ored_guard.t option * (* alias *) Var.t) list
      | Seq2 of t * t
      | SelectImm of
          (*branch ORed guards*)
          (Ored_guard.t * t) list
          * (* else guards *)
          Ored_guard.t option
      | DoWhile of t * Var.t
      | Loop of t
    [@@deriving sexp_of]
  end

  let glom l ~f:is_glomable =
    List.folding_map
      ~init:(0, List.hd_exn l |> is_glomable)
      l
      ~f:(fun (i, last_is_m) e ->
        let is_m = is_glomable e in
        let i = if (not is_m) || ((not last_is_m) && is_m) then i + 1 else i in
        ((i, is_m), (i, e)))
    |> Int.Map.of_alist_multi |> Map.data

  let rec flatten rui =
    match rui with
    | Proc.Op v -> Pre_synth.Op v
    | Seq ruis ->
        (* first, go through and join segments that can be implemented as a
           Opt_guarded_op_seq *)
        let ruis =
          glom ruis ~f:(fun rui ->
              match rui with
              | Proc.Op _ | Proc.SelectImm ([ (_, Op _) ], _) -> true
              | _ -> false)
          |> List.concat_map ~f:(fun l ->
                 if List.length l < 2 then List.map l ~f:flatten
                 else
                   let l =
                     List.map l ~f:(fun rui ->
                         match rui with
                         | Proc.Op alias -> (None, alias)
                         | Proc.SelectImm ([ (g, Op alias) ], _) ->
                             (Some g, alias)
                         | _ -> failwith "unreachable")
                   in
                   [ Pre_synth.Opt_guarded_op_seq l ])
        in
        List.reduce_exn ruis ~f:(fun a b -> Seq2 (a, b))
    | SelectImm (on_branches, else_guards) ->
        SelectImm
          (List.map on_branches ~f:(fun (g, e) -> (g, flatten e)), else_guards)
    | DoWhile (rui, g) -> DoWhile (flatten rui, g)
    | Loop rui -> Loop (flatten rui)

  module Synth = struct
    (* TODO Add direct support for "Op-seq"s and for "conditionaled Op-seq"s *)
    type t =
      | Op of Var.t
      (* A sequence consisting of Op!op and [ g -> Op!op [] else -> skip ] *)
      | Opt_guarded_op_seq of (* guard *) (Var.t option * (*alias *) Var.t) list
      | Seq2 of t * t
      | SelectImm of (Var.t * t) list * Var.t option
      | DoWhile of t * Var.t
      | Loop of t
    [@@deriving sexp_of]
  end

  module Alias_with_ctrl = struct
    type t = { alias : Var.t; ctrl : Var.t } [@@deriving sexp_of, fields]
  end

  let lower presynth ~new_chan =
    let new_ored_guard guards =
      match Nonempty_list.to_list guards with
      | [] -> failwith "unreachable"
      | [ g ] -> (g, [])
      | guards ->
          let v = new_chan 1 in
          let expr =
            List.map guards ~f:(fun v -> F_expr.Var v)
            |> List.reduce_exn ~f:(fun a b -> F_expr.BitOr (a, b))
          in
          let ctrl_proc = MultiAssign (FBlock.create1 v expr) in
          (v, [ ctrl_proc ])
    in
    let rec lower n =
      match n with
      | Pre_synth.Op v -> (Synth.Op v, [])
      | Opt_guarded_op_seq l ->
          let l, dflows =
            List.map l ~f:(fun (gs, alias) ->
                let g, dflows =
                  match gs with
                  | Some gs ->
                      let g, dflows = new_ored_guard gs in
                      (Some g, dflows)
                  | None -> (None, [])
                in
                ((g, alias), dflows))
            |> List.unzip
          in
          (Opt_guarded_op_seq l, List.concat dflows)
      | Seq2 (a, b) ->
          let a, a_dflows = lower a in
          let b, b_dflows = lower b in
          (Seq2 (a, b), a_dflows @ b_dflows)
      | SelectImm (on_branches, else_guards) ->
          let on_branches, dflows =
            List.map on_branches ~f:(fun (gs, n) ->
                let g, dflows = new_ored_guard gs in
                let n, more_dflows = lower n in
                ((g, n), dflows @ more_dflows))
            |> List.unzip
          in
          let else_guard, more_dflows =
            match else_guards with
            | None -> (None, [])
            | Some else_guards ->
                let g, dflows = new_ored_guard else_guards in
                (Some g, dflows)
          in
          (SelectImm (on_branches, else_guard), List.concat dflows @ more_dflows)
      | DoWhile (rui, g) ->
          let rui, dflows = lower rui in
          (DoWhile (rui, g), dflows)
      | Loop rui ->
          let rui, dflows = lower rui in
          (Loop rui, dflows)
    in
    lower presynth

  let merge_bool_guard s (b1, b2) v = [ Merge (Idx s, [ b1; b2 ], v) ]
  let split_bool_guard s v (b1, b2) = [ Split (Idx s, v, [ b1; b2 ]) ]
  let assign dst e = MultiAssign (FBlock.create1 dst e)

  let synth_op alias ~new_chan ~dir:_ =
    let ctrl = new_chan 1 in
    let s_ = new_chan 1 in
    let dflow =
      [
        Buff1 (ctrl, s_, Some CInt.zero);
        assign s_ (BitXor (Const CInt.one, Var ctrl));
      ]
    in
    ({ Alias_with_ctrl.alias; ctrl }, dflow)

  let synth_opt_guarded_op_seq guarded_aliases ~new_chan ~dir =
    let ct = List.length guarded_aliases in
    let ct_const = F_expr.Const (CInt.of_int ct) in
    let n_width = Int.ceil_log2 (ct + 1) in

    let a_chan = new_chan (List.hd_exn guarded_aliases |> snd).Var.bitwidth in
    let b_chan = new_chan 1 in

    let n = new_chan n_width in

    (* [n=0 -> G0?g [] n=1 -> G1?g [] ... [] n=L -> skip]; *)
    let g = new_chan 1 in
    let dflow1 =
      let dflows, gis =
        List.map guarded_aliases ~f:(fun (g_chan, _) ->
            match g_chan with
            | Some g_chan -> ([], g_chan)
            | None ->
                let gi = new_chan 1 in
                ([ assign gi F_expr.(Const CInt.one) ], gi))
        |> List.unzip
      in
      let gL = new_chan 1 in
      let assign_L = assign gL (Const CInt.zero) in
      let gis = gis @ [ gL ] in
      assert (List.length gis |> Int.equal (ct + 1));
      [ Merge (Idx n, gis, g); assign_L ] @ List.concat dflows
    in

    (* [(g=0 | n=L) -> skip [] else -> [n=0 -> A0!x [] ... n=L-1 -> A{L-1}!x]];
       OR *)
    (* [(g=0 | n=L) -> skip [] else -> [n=0 -> A0?x [] ... n=L-1 -> A{L-1}?x]];
       A!x; *)
    let dflow2 =
      let n1 = new_chan n_width in
      let sg = new_chan 1 in
      let op_proc =
        match dir with
        | `Send ->
            let ais = List.map guarded_aliases ~f:snd in
            assert (List.length ais |> Int.equal ct);
            Merge (Idx n1, ais, a_chan)
        | `Read ->
            let ais =
              List.map guarded_aliases ~f:(fun (_, alias) -> Some alias)
            in
            Split (Idx n1, a_chan, ais)
      in
      let sg_expr = F_expr.(BitOr (Eq0 (Var g), Eq (Var n, ct_const))) in
      [ MultiAssign (FBlock.create1 sg sg_expr); op_proc ]
      @ split_bool_guard sg n (Some n1, None)
    in

    (* [ (n=L | g=1) -> B!(n!=L) [] else -> skip ] *)
    let dflow3 =
      let n1 = new_chan n_width in
      let sg = new_chan 1 in
      let sg_expr = F_expr.(BitOr (Var g, Eq (Var n, ct_const))) in
      let b_expr = F_expr.(Ne (Var n1, ct_const)) in
      [
        MultiAssign (FBlock.create1 sg sg_expr);
        MultiAssign (FBlock.create1 b_chan b_expr);
      ]
      @ split_bool_guard sg n (None, Some n1)
    in

    (* n := ( n=L ? 0 : n+1 ); *)
    let n' = new_chan n_width in
    let dflow4 =
      let n_expr =
        F_expr.(
          BitOr
            ( Mul (Eq (Var n, ct_const), Const CInt.zero),
              Mul (Ne (Var n, ct_const), Add (Var n, Const CInt.one)) ))
      in
      [ MultiAssign (FBlock.create1 n' n_expr) ]
    in

    (* finally, we must wrap `n` back around to the top *)
    let dflow_wrap = [ Buff1 (n, n', Some CInt.zero) ] in
    ( { Alias_with_ctrl.alias = a_chan; ctrl = b_chan },
      dflow1 @ dflow2 @ dflow3 @ dflow4 @ dflow_wrap )

  let synth_seq2 ~(rui1 : Alias_with_ctrl.t) ~(rui2 : Alias_with_ctrl.t)
      ~new_chan ~dir =
    assert (Int.equal rui1.alias.bitwidth rui2.alias.bitwidth);
    assert (Int.equal rui1.ctrl.bitwidth 1);
    assert (Int.equal rui2.ctrl.bitwidth 1);
    let a = new_chan rui1.alias.bitwidth in
    let b_chan = new_chan 1 in
    let s = new_chan 1 in
    (* [ ~s -> b1?v [] s -> b2?v ] *)
    let v = new_chan 1 in
    let dflow1 = merge_bool_guard s (rui1.ctrl, rui2.ctrl) v in
    (* [ ~ (s | v) -> skip [] (s | v) -> b!v ] *)
    let dflow2 =
      let g = new_chan 1 in
      assign g (F_expr.BitOr (Var s, Var v))
      :: split_bool_guard g v (None, Some b_chan)
    in

    (* EITHER [ ~v -> s := ~s [] v -> a?x; [ ~s -> a1!x [] s -> a2!x ] ] OR [ ~v
       -> s := ~s [] v -> [ ~s -> a1?x [] s -> a2?x ] a!x; ] *)
    let s' = new_chan 1 in
    let dflow3 =
      let s0 = new_chan 1 in
      let s1 = new_chan 1 in
      let s0' = new_chan 1 in
      let op_procs =
        match dir with
        | `Read -> split_bool_guard s1 a (Some rui1.alias, Some rui2.alias)
        | `Send -> merge_bool_guard s1 (rui1.alias, rui2.alias) a
      in
      [
        split_bool_guard v s (Some s0, Some s1);
        [ assign s0' (F_expr.BitXor (Var s0, Const CInt.one)) ];
        merge_bool_guard v (s0', s1) s';
        op_procs;
      ]
      |> List.concat
    in
    (* finally, we must wrap `s` back around to the top *)
    let dflow_wrap = [ Buff1 (s, s', Some CInt.zero) ] in
    ( { Alias_with_ctrl.alias = a; ctrl = b_chan },
      dflow1 @ dflow2 @ dflow3 @ dflow_wrap )

  let synth_select_imm guards aliases ~new_chan ~dir =
    let ab_list = aliases in
    let a_chan =
      new_chan
        (List.filter_opt aliases |> List.hd_exn).Alias_with_ctrl.alias.bitwidth
    in
    let b_chan = new_chan 1 in
    let g_width = List.length guards in
    let v = new_chan 1 in
    let g = new_chan g_width in

    (* [ ~v -> c?g [] v -> skip ] *)
    let g' = new_chan g_width in
    let dflow1 =
      let g1 = new_chan g_width in
      let c = new_chan g_width in
      let c_expr =
        F_expr.Concat (List.map guards ~f:(fun e -> (F_expr.Var e, 1)))
      in
      [
        split_bool_guard v g (None, Some g1);
        (* pack guards into c. TODO get around this somehow to expose this to
           the optimizer better. *)
        [ assign c c_expr ];
        merge_bool_guard v (c, g1) g';
      ]
      |> List.concat
    in

    (* [ g{i}=1 -> B_i?v ]; B!v; *)
    let v' = new_chan 1 in
    let dflow2 =
      let dflows, vi's =
        List.map ab_list ~f:(fun o ->
            let vi' = new_chan 1 in
            let dflow =
              match o with
              | None -> assign vi' (Const CInt.zero)
              | Some rui_i -> assign vi' (Var rui_i.ctrl)
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
                    | Some rui_i -> assign vi' (Var rui_i.alias)
                  in
                  (dflow, vi'))
              |> List.unzip
            in
            Merge (One_hot g1', ais, a_chan) :: dflows
        | `Read ->
            let ais =
              List.map ab_list ~f:(Option.map ~f:Alias_with_ctrl.alias)
            in
            [ Split (One_hot g1', a_chan, ais) ]
      in
      split_bool_guard v' g' (None, Some g1') @ op_procs
    in

    (* finally, we must wrap `v` and `g` back around to the top *)
    let dflow_wrap =
      [ Buff1 (v, v', Some CInt.zero); Buff1 (g, g', Some CInt.one) ]
    in
    let dflow = dflow1 @ dflow2 @ dflow3 @ dflow_wrap in
    ({ Alias_with_ctrl.alias = a_chan; ctrl = b_chan }, dflow)

  let synth_do_while (rui : Alias_with_ctrl.t) guard_chan ~new_chan ~dir:_ =
    let b_chan = new_chan 1 in
    let v = new_chan 1 in
    let g = new_chan 1 in
    (* B1?v ; [ ~v -> C?g [] v -> skip ] *)
    let g' = new_chan 1 in
    let dflow1 =
      let g1 = new_chan 1 in
      [
        [ assign v (Var rui.ctrl) ];
        split_bool_guard v g (None, Some g1);
        merge_bool_guard v (guard_chan, g1) g';
      ]
      |> List.concat
    in
    (* [ ~(v | ~g) -> skip [] (v | ~g) -> B!b ]; *)
    let dflow2 =
      let e = new_chan 1 in
      assign e (BitOr (Var v, Eq0 (Var g)))
      :: split_bool_guard e v (None, Some b_chan)
    in

    (* finally, we must wrap `g` back around to the top *)
    let dflow_wrap = [ Buff1 (g, g', Some CInt.zero) ] in
    let dflows = dflow1 @ dflow2 @ dflow_wrap in
    ({ Alias_with_ctrl.alias = rui.alias; ctrl = b_chan }, dflows)

  let synth_loop rui ~new_chan ~dir:_ =
    let b_chan = new_chan 1 in
    ( { Alias_with_ctrl.alias = rui.Alias_with_ctrl.alias; ctrl = b_chan },
      [ assign b_chan (Const CInt.one) ] )

  let rec synth rui ~dir ~new_chan =
    let synth rui = synth rui ~dir ~new_chan in
    match rui with
    | Synth.Op alias ->
        let rui, dflows = synth_op alias ~new_chan ~dir in
        assert (Int.equal rui.alias.bitwidth alias.bitwidth);
        assert (Int.equal rui.ctrl.bitwidth 1);
        (rui, dflows)
    | Opt_guarded_op_seq guarded_aliases ->
        let alias, dflows =
          synth_opt_guarded_op_seq guarded_aliases ~new_chan ~dir
        in
        (alias, dflows)
    | Seq2 (rui1, rui2) ->
        let rui1, dflows1 = synth rui1 in
        let rui2, dflows2 = synth rui2 in
        let rui, more_dflows = synth_seq2 ~rui1 ~rui2 ~new_chan ~dir in
        assert (Int.equal rui.alias.bitwidth rui1.alias.bitwidth);
        assert (Int.equal rui.ctrl.bitwidth 1);
        (rui, dflows1 @ dflows2 @ more_dflows)
    | SelectImm (some_branches, else_guard) ->
        let some_branches, dflows =
          List.map some_branches ~f:(fun (guard, rui) ->
              let alias, dflows = synth rui in
              ((guard, alias), dflows))
          |> List.unzip
        in
        let else_ = else_guard in
        let guards = List.map some_branches ~f:fst @ Option.to_list else_ in
        let ab_list =
          List.map some_branches ~f:(fun (_, alias) -> Some alias)
          @ (Option.map else_ ~f:(fun _ -> None) |> Option.to_list)
        in
        let rui, more_dflows = synth_select_imm guards ab_list ~new_chan ~dir in
        (* assert (Int.equal rui.alias.bitwidth alias.bitwidth); *)
        assert (Int.equal rui.ctrl.bitwidth 1);
        (rui, List.concat dflows @ more_dflows)
    | DoWhile (rui1, g) ->
        let rui1, dflows = synth rui1 in
        let rui, more_dflows = synth_do_while rui1 g ~new_chan ~dir in
        assert (Int.equal rui.alias.bitwidth rui1.alias.bitwidth);
        assert (Int.equal rui.ctrl.bitwidth 1);
        (rui, dflows @ more_dflows)
    | Loop rui1 ->
        let rui1, dflows = synth rui1 in
        let rui, more_dflows = synth_loop rui1 ~new_chan ~dir in
        assert (Int.equal rui.alias.bitwidth rui1.alias.bitwidth);
        assert (Int.equal rui.ctrl.bitwidth 1);
        (rui, dflows @ more_dflows)

  let synthasize_rui rui ~new_chan ~dir =
    let rui = optimize rui in
    let rui =
      match rui with
      | None -> failwith "unreachable channel -  TODO"
      | Some rui -> flatten rui
    in
    let rui, dflows = lower rui ~new_chan in
    let alias, more_dflows = synth rui ~new_chan ~dir in
    (alias, dflows @ more_dflows)
end

let dflow_of_stf proc =
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in
  let new_alias (a : Var.t) = new_chan a.bitwidth in

  let id_of_var = Stf.Var.Table.create () in
  let of_v v =
    Hashtbl.find_or_add id_of_var v ~default:(fun () -> new_chan v.bitwidth)
  in

  let of_e e = F_expr.map_vars e ~f:(fun v -> of_v v) in
  let of_e' (e : Stf.Var.t F_expr.t) =
    let bitwidth = F_expr.bitwidth e ~bits_of_var:(fun v -> v.bitwidth) in
    let v = new_chan bitwidth in
    (MultiAssign (FBlock.create1 v (of_e e)), v)
  in

  (* returns a tuple (dflow, alias_map) where alias_map is a map { Chan_end.t ->
     (alias, ctrl)} *)
  let rec of_stmt n =
    match n with
    | Stf.Stmt.Nop -> ([], Chan_end.Map.empty)
    | Assign (v, e) ->
        ([ MultiAssign (FBlock.create1 (of_v v) (of_e e)) ], Chan_end.Map.empty)
    | Send (c, e) ->
        let tmp = new_chan c.bitwidth in
        let alias = new_chan c.bitwidth in
        let ctrl_proc_map =
          Chan_end.Map.singleton (Chan_end.Send c) (Rui.Proc.Op alias)
        in
        ( [ MultiAssign (FBlock.create1 tmp (of_e e)); Buff1 (alias, tmp, None) ],
          ctrl_proc_map )
    | Read (c, v) ->
        let alias = new_chan c.bitwidth in
        let ctrl_proc_map =
          Chan_end.Map.singleton (Chan_end.Read c) (Rui.Proc.Op alias)
        in
        ([ Buff1 (of_v v, alias, None) ], ctrl_proc_map)
    | Seq stmts ->
        let stmts, ctrl_proc_maps = List.map stmts ~f:of_stmt |> List.unzip in
        (* This should preserve the order of the original maps? *)
        let ctrl_proc_map =
          List.map ctrl_proc_maps ~f:Map.to_alist
          |> List.concat |> Chan_end.Map.of_alist_multi
          |> Map.map ~f:(fun ctrl_procs -> Rui.Proc.Seq ctrl_procs)
        in
        (List.concat stmts, ctrl_proc_map)
    | Par (splits, stmts, merges) ->
        let splits =
          List.concat_map splits ~f:(fun split ->
              List.filter_map split.out_vs ~f:Fn.id
              |> List.map ~f:(fun out_v ->
                     MultiAssign
                       (FBlock.create1 (of_v out_v) (Var (of_v split.in_v)))))
        in
        let merges =
          List.concat_map merges ~f:(fun merge ->
              List.filter_map merge.in_vs ~f:Fn.id
              |> List.map ~f:(fun in_v ->
                     MultiAssign
                       (FBlock.create1 (of_v merge.out_v) (Var (of_v in_v)))))
        in
        let stmts, ctrl_proc_maps = List.map stmts ~f:of_stmt |> List.unzip in
        let ctrl_proc_map =
          List.reduce ctrl_proc_maps ~f:(fun m1 m2 ->
              Map.merge_skewed m1 m2 ~combine:(fun ~key:_ _ _ ->
                  failwith
                    "Multiple branches interact with the same end of the same \
                     channel. This is a bug. It should have been cought in \
                     is_dflowable"))
          |> Option.value ~default:Chan_end.Map.empty
        in
        (List.concat [ splits; merges; List.concat stmts ], ctrl_proc_map)
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
        let branches, ctrl_proc_maps =
          List.map branches ~f:of_stmt |> List.unzip
        in
        let ctrl_proc_map =
          List.concat_map ctrl_proc_maps ~f:Map.keys
          |> Chan_end.Set.of_list
          |> Map.of_key_set ~f:(fun k ->
                 let some_branches, else_guards =
                   List.zip_exn guards ctrl_proc_maps
                   |> List.partition_map ~f:(fun (g, ctrl_proc_map) ->
                          match Map.find ctrl_proc_map k with
                          | Some ctrl_proc ->
                              Either.First (Nonempty_list.singleton g, ctrl_proc)
                          | None -> Either.Second g)
                 in
                 let else_guards = Nonempty_list.of_list_opt else_guards in
                 Rui.Proc.SelectImm (some_branches, else_guards))
        in
        let dflows =
          List.concat [ guard_procs; splits; merges; List.concat branches ]
        in
        (dflows, ctrl_proc_map)
    | DoWhile (phis, stmt, guard) ->
        let is_infinite =
          match guard with Const c -> CInt.equal c CInt.one | _ -> false
        in
        (* TODO add special case for infinite loop *)
        let guard_proc, guard = of_e' guard in
        let prev_guard = new_alias guard in
        let copy_init = Buff1 (prev_guard, guard, Some CInt.zero) in
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
              List.filter_opt [ top; bottom ])
        in
        let stmts, ctrl_proc_map = of_stmt stmt in
        let ctrl_proc_map =
          Map.map ctrl_proc_map ~f:(fun ctrl_proc ->
              if is_infinite then Rui.Proc.Loop ctrl_proc
              else Rui.Proc.DoWhile (ctrl_proc, guard))
        in
        let dflow = guard_proc :: copy_init :: (phis @ stmts) in
        (dflow, ctrl_proc_map)
  in
  let dflow, ctrl_proc_map = of_stmt proc.Stf.Proc.stmt in

  let alias_map, ctrl_procs =
    let alias_map, ctrl_procs =
      let m =
        Map.mapi ctrl_proc_map ~f:(fun ~key:chan_end ~data:rui ->
            let dir = match chan_end with Send _ -> `Send | Read _ -> `Read in
            Rui.synthasize_rui rui ~new_chan ~dir)
      in
      (Map.map m ~f:fst, Map.map m ~f:snd)
    in
    (alias_map, Map.data ctrl_procs |> List.concat)
  in
  let dflow = dflow @ ctrl_procs in
  let iports =
    List.map proc.iports ~f:(fun (interproc, chan) ->
        let alias =
          match Map.find alias_map (Read chan) with
          | Some rui -> rui.alias
          | None -> new_chan chan.bitwidth
        in
        (interproc, alias))
  in
  let oports, more_dflows =
    List.map proc.oports ~f:(fun (interproc, chan) ->
        (* TODO jsut create a dummy stream of zeros? This is techniqually an
           invalid program, since each output must receive an infinite stream of
           tokens, yet no tokens are available on this stream *)
        let alias, dflows =
          match Map.find alias_map (Send chan) with
          | Some rui -> (rui.alias, [])
          | None ->
              let tmp = new_chan chan.bitwidth in
              (tmp, [ MultiAssign (FBlock.create1 tmp (F_expr.Const CInt.zero)) ])
        in
        ((interproc, alias), dflows))
    |> List.unzip
  in
  let dflow = dflow @ List.concat more_dflows in
  let proc = { Proc.stmt = dflow; iports; oports } in
  validate proc;
  proc

let var_ids dflows iports oports =
  [
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> FBlock.ins fblock @ FBlock.outs fblock
        | Split (g, v, os) -> (v :: Guard.ids g) @ List.filter_opt os
        | Merge (g, ins, v) -> (v :: Guard.ids g) @ ins
        | Buff1 (dst, src, _) -> [ dst; src ]);
    List.map iports ~f:snd;
    List.map oports ~f:snd;
  ]
  |> List.concat |> Var.Set.of_list

let eliminate_dead_code proc =
  let { Proc.stmt = dflows; iports; oports } = proc in
  let dependecies_of_id =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock ->
            FBlock.deps_of_outs fblock
            |> List.concat_map ~f:(fun (dst, ins) ->
                   List.map ins ~f:(fun in_ -> (dst, in_)))
        | Split (g, v, os) ->
            List.filter_opt os
            |> List.concat_map ~f:(fun o ->
                   (o, v) :: List.map (Guard.ids g) ~f:(fun g -> (o, g)))
        | Merge (g, ins, v) -> List.map (Guard.ids g @ ins) ~f:(fun i -> (v, i))
        | Buff1 (dst, src, _) -> [ (dst, src) ])
    |> Var.Map.of_alist_multi
  in
  let alive = Var.Table.create () in
  let queue = Queue.create () in
  List.iter oports ~f:(fun (_, port) ->
      Hashtbl.find_or_add alive port ~default:(fun () ->
          Queue.enqueue queue port));
  while Queue.length queue > 0 do
    let id = Queue.dequeue_exn queue in
    let deps = Map.find dependecies_of_id id |> Option.value ~default:[] in
    List.iter deps ~f:(fun id ->
        Hashtbl.find_or_add alive id ~default:(fun () -> Queue.enqueue queue id))
  done;
  let alive = Hashtbl.keys alive |> Var.Set.of_list in
  let dflows =
    List.filter_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock ->
            (* we cant just eliminate unread variables because that might change
               the dependencies of the MultiAssign block *)
            (* TODO the better way to do this is to have an explicit list of
               dependencies for the multiassign block *)
            let fblock =
              FBlock.filter_outs fblock ~f:(fun dst -> Set.mem alive dst)
            in
            if FBlock.outs fblock |> List.is_empty then None
            else Some (MultiAssign fblock)
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
        | Buff1 (dst, src, init) ->
            if Set.mem alive dst then Some (Buff1 (dst, src, init)) else None)
  in
  { Proc.stmt = dflows; oports; iports }

let eliminate_repeated_vars proc =
  let { Proc.stmt = dflows; iports; oports } = proc in
  let src_of_dst =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> (
            let get_copy fblock =
              match FBlock.expr_list fblock with
              | [ (dst, Var src) ] ->
                  if FBlock.ins fblock |> List.length |> Int.equal 1 then
                    Some (dst, src)
                  else None
              | _ -> None
            in
            match get_copy fblock with
            | Some (dst, src) -> [ (dst, src) ]
            | None ->
                (* Note that a `dst <- src` inside of a multi-assign block may
                   not be optimized, as the block must wait on _every_ read to
                   complete before an expression exicutes, so the `dst <- src`
                   is not the same as `src` *)
                (* we may, however, deduplicate repeated expressions *)
                FBlock.dup_ids fblock
                |> List.concat_map ~f:(fun l ->
                       match l with
                       | [] | [ _ ] -> failwith "unreachable"
                       | h :: l -> List.map l ~f:(fun x -> (x, h))))
        | _ -> [])
    |> Var.Map.of_alist_exn
  in

  (* This is probably way slower than needed *)
  let true_src ~dst ~src =
    let rec h x =
      if Var.equal x dst then None
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
        | MultiAssign fblock ->
            let fblock = FBlock.map_ins fblock ~f:of_v in
            MultiAssign fblock
        | Split (g, v, os) ->
            Split (Guard.map ~f:of_v g, of_v v, List.map os ~f:of_vo)
        | Merge (g, ins, v) ->
            Merge (Guard.map ~f:of_v g, List.map ~f:of_v ins, of_v v)
        | Buff1 (dst, src, init) -> Buff1 (of_v dst, of_v src, init))
  in
  let iports = List.map iports ~f:(fun (i, v) -> (i, of_v v)) in
  let oports = List.map oports ~f:(fun (i, v) -> (i, of_v v)) in
  { Proc.stmt = dflows; oports; iports }

(* A compilcation when optimizing dataflow is that one cannot, in general,
   safely ADD OR REMOVE reads for a node. In particular, this means that
   optimizing expressions is hard, since an expression still needs to read a
   token for timing reasons even if it never looks at it. Moreover, fusing two
   assigns into a block is in general not allowed, because the MultiAssign block
   will only fire one it has received all the necassary tokens.

   However, the following three transformations are always valid. 1) If two
   nodes read the same set of tokens, they may be merged together. 2) If one
   node is the only node that reads the output of a second node, the second node
   may be merged into the first. 3) We may duplicate a node, and may assign some
   readers of the original node to read the duplicate node instead.

   Here I implement the first and second optimization. More general
   breaking/clusering work could be useful *)

(* Transofmation #1 *)
let cluster_same_reads { Proc.stmt = dflows; iports; oports } =
  let fblocks, non_assigns =
    List.partition_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> First fblock
        | _ -> Second dflow)
  in

  let multi_assigns =
    List.map fblocks ~f:(fun fblock ->
        let reads = FBlock.ins fblock |> Var.Set.of_list in
        (reads, fblock))
    |> Var_set.Map.of_alist_multi |> Map.to_alist
    |> List.concat_map ~f:(fun (reads, fblocks) ->
           if Set.is_empty reads then
             List.map fblocks ~f:(fun fblock -> MultiAssign fblock)
           else
             let fblock = List.reduce_exn fblocks ~f:FBlock.merge_same_reads in

             [ MultiAssign fblock ])
  in

  { Proc.stmt = multi_assigns @ non_assigns; oports; iports }
(* let connected_components verts edges = let verts = Map.of_key_set verts
   ~f:Union_find.create in List.iter edges ~f:(fun (a, b) -> Union_find.union
   (Map.find_exn verts a) (Map.find_exn verts b)); let clusters = Map.to_alist
   verts |> List.fold ~init:[] ~f:(fun clusters (_, id_class) -> match List.find
   clusters ~f:(fun cluster -> Union_find.same_class cluster id_class) with |
   Some _ -> clusters | None -> id_class :: clusters) |> List.mapi ~f:(fun i
   cluster -> (cluster, i)) in let vert_clusters = Map.map verts ~f:(fun
   id_class -> List.find_exn clusters ~f:(fun (cluster, _) ->
   Union_find.same_class cluster id_class) |> snd) |> Map.to_alist |> List.map
   ~f:(fun (vert, id) -> (id, vert)) |> Int.Map.of_alist_multi |> Map.data |>
   List.map ~f:(fun cluster -> (* The topologically sort the cluster, so that
   `(a,b) in edges` implies that `a is before b` in the returned list *) (* TODO
   *) cluster) in vert_clusters *)

(* Transofmation #2 *)
(* let cluster_fuse_chains { Proc.stmt = dflows; iports; oports } = let dflows =
   List.mapi dflows ~f:(fun id dflow -> (id, dflow)) in let multi_assigns,
   non_assigns = List.partition_map dflows ~f:(fun (id, dflow) -> match dflow
   with | MultiAssign multi_assign -> First (id, multi_assign) | _ -> Second
   dflow) in

   let cluster_of_output = List.concat_map multi_assigns ~f:(fun (cluster_id,
   multi_assign) -> List.map multi_assign ~f:fst |> Var.Set.of_list |>
   Set.to_list |> List.map ~f:(fun dflow_id -> (dflow_id, cluster_id))) |>
   Var.Map.of_alist_exn in (* let read_cluster_of_id = Map.map multi_assigns
   ~f:(fun multi_assign -> List.concat_map multi_assign ~f:(fun (_, e) ->
   F_expr.var_ids) |> List.filter_map ~f:(Map.find id_of_output) |>
   Int.Set.of_list) in *) let read_ct_of_cluster = List.map dflows ~f:(fun (_,
   dflow) -> (match dflow with | MultiAssign assigns -> List.concat_map assigns
   ~f:(fun (_, e) -> F_expr.var_ids e) | Split (g, v, os) -> (v :: Guard.ids g) @
   List.filter_opt os | Merge (g, ins, v) -> (v :: Guard.ids g) @ ins | Buff1
   (dst, src, _) -> [ dst; src ]) |> List.filter_map ~f:(Map.find
   cluster_of_output) |> Int.Set.of_list) |> List.concat_map ~f:Set.to_list |>
   List.map ~f:(fun v -> (v, ())) |> Int.Map.of_alist_multi |> Map.map
   ~f:List.length in

   let fuses = List.concat_map multi_assigns ~f:(fun (id_i, _) ->
   List.filter_map multi_assigns ~f:(fun (id_o, multi_assign) -> let
   reads_of_id_o = List.concat_map multi_assign ~f:(fun (_, e) -> F_expr.var_ids
   e) |> List.filter_map ~f:(Map.find cluster_of_output) |> Int.Set.of_list in
   let o_only_reader_of_i = Int.equal 1 (Map.find read_ct_of_cluster id_i |>
   Option.value ~default:0) && Set.mem reads_of_id_o id_i in let
   o_only_reads_of_i = List.equal Int.equal (Set.to_list reads_of_id_o) [ id_i ]
   in if o_only_reader_of_i || o_only_reads_of_i then Some (id_i, id_o) else
   None)) in let clusters = let ids = List.map multi_assigns ~f:fst |>
   Int.Set.of_list in connected_components ids fuses in

   let multi_assign_of_id = Int.Map.of_alist_exn multi_assigns in let
   multi_assigns = List.map clusters ~f:(fun cluster -> let multi_assigns =
   List.map cluster ~f:(Map.find_exn multi_assign_of_id) in (* TODO currently
   this is incorrect. We must rewrite later mutliassigns to use the epression of
   preceding multiassigns (instead of the raw variables) *) MultiAssign
   (List.concat multi_assigns)) in { Proc.stmt = multi_assigns @ non_assigns;
   oports; iports } *)

let cluster_fuse_chains { Proc.stmt = dflows; iports; oports } =
  (* TODO improve the performance of this code! *)
  let fblocks, non_assigns =
    List.partition_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> First fblock
        | _ -> Second dflow)
  in
  let do_one_fuse fblocks =
    let fblocks = List.mapi fblocks ~f:(fun i fblock -> (i, fblock)) in
    let cluster_of_output =
      List.concat_map fblocks ~f:(fun (cluster_id, fblock) ->
          FBlock.outs fblock
          |> List.map ~f:(fun dflow_id -> (dflow_id, cluster_id)))
      |> Var.Map.of_alist_exn
    in
    let read_ct_of_cluster =
      List.map dflows ~f:(fun dflow ->
          (match dflow with
          | MultiAssign fblock -> FBlock.ins fblock
          | Split (g, v, os) -> (v :: Guard.ids g) @ List.filter_opt os
          | Merge (g, ins, v) -> (v :: Guard.ids g) @ ins
          | Buff1 (dst, src, _) -> [ dst; src ])
          |> List.filter_map ~f:(Map.find cluster_of_output)
          |> Int.Set.of_list)
      |> List.concat_map ~f:Set.to_list
      |> List.map ~f:(fun v -> (v, ()))
      |> Int.Map.of_alist_multi |> Map.map ~f:List.length
    in

    let fuses =
      List.concat_map fblocks ~f:(fun (id_i, _) ->
          List.filter_map fblocks ~f:(fun (id_o, fblock) ->
              let reads_of_id_o =
                FBlock.ins fblock
                |> List.filter_map ~f:(Map.find cluster_of_output)
                |> Int.Set.of_list
              in
              let o_only_reader_of_i =
                Int.equal 1
                  (Map.find read_ct_of_cluster id_i |> Option.value ~default:0)
                && Set.mem reads_of_id_o id_i
              in
              let o_only_reads_of_i =
                List.equal Int.equal (Set.to_list reads_of_id_o) [ id_i ]
              in
              if o_only_reader_of_i || o_only_reads_of_i then Some (id_i, id_o)
              else None))
    in
    match fuses with
    | [] -> (List.map fblocks ~f:snd, false)
    | (i, o) :: _ ->
        let l =
          List.partition3_map fblocks ~f:(fun x ->
              let id, _ = x in
              if Int.equal id i then `Fst x
              else if Int.equal id o then `Snd x
              else `Trd x)
        in
        let i, o, others =
          match l with
          | [ (_, i) ], [ (_, o) ], others -> (i, o, List.map others ~f:snd)
          | _ -> failwith "TODO"
        in

        (FBlock.append i o :: others, true)
  in
  let fblocks = ref fblocks in
  while
    let ma, was_change = do_one_fuse !fblocks in
    fblocks := ma;
    was_change
  do
    ()
  done;
  let fblocks = List.map !fblocks ~f:(fun fblock -> MultiAssign fblock) in

  { Proc.stmt = fblocks @ non_assigns; oports; iports }

let normalize_guards (proc : Proc.t) =
  let next_id =
    let biggest_id =
      var_ids proc.stmt proc.iports proc.oports
      |> Set.to_list
      |> List.map ~f:(fun v -> v.id)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    ref (biggest_id + 1)
  in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in
  let dflows = proc.stmt in
  let guards =
    List.filter_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign _ -> None
        | Buff1 _ -> None
        | Merge (g, _, _) -> Some g
        | Split (g, _, _) -> Some g)
    |> Guard.Set.of_list
    |> Guard.Map.of_key_set ~f:(fun g ->
           match g with
           | Idx g -> (Guard.Idx g, None)
           | One_hot g ->
               let new_g = new_chan (Int.ceil_log2 g.bitwidth) in
               let expr = F_expr.Log2OneHot (Var g) in
               let assign = MultiAssign (FBlock.create1 new_g expr) in
               (Idx new_g, Some assign)
           | Bits gs ->
               let bits = Int.ceil_log2 (List.length gs) in
               let new_g = new_chan bits in
               let gs = List.map gs ~f:(fun g -> (F_expr.Var g, 1)) in
               let assign =
                 MultiAssign (FBlock.create1 new_g (Log2OneHot (Concat gs)))
               in
               (Idx new_g, Some assign))
  in
  let procs = Map.data guards |> List.filter_map ~f:snd in
  let guards = Map.map guards ~f:fst in
  let dflows =
    List.map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign assigns -> MultiAssign assigns
        | Buff1 (dst, src, v) -> Buff1 (dst, src, v)
        | Merge (g, srcs, dst) -> Merge (Map.find_exn guards g, srcs, dst)
        | Split (g, src, dsts) -> Split (Map.find_exn guards g, src, dsts))
  in
  let dflows = dflows @ procs in
  { Proc.stmt = dflows; iports = proc.iports; oports = proc.oports }

let propogate_consts { Proc.stmt = dflows; iports; oports } =
  { Proc.stmt = dflows; iports; oports }

(* let const_vars = List.concat_map dflows ~f:(fun dflow -> match dflow with |
   MultiAssign l -> if List.concat_map l ~f:(fun (_, e) -> F_expr.var_ids e) |>
   List.is_empty then l else [] | _ -> []) |> Var.Map.of_alist_exn |> Map.map
   ~f:eval_const_expr in let dflows = List.concat_map dflows ~f:(fun dflow ->
   match dflow with | MultiAssign assigns -> let assigns = List.map assigns
   ~f:(fun (dst, e) -> let e = F_expr.bind_vars e ~f:(fun v -> match Map.find
   const_vars v with | Some c -> Const c | None -> Var v) in (dst, e)) in [
   MultiAssign assigns ] | Buff1 (dst, src, init) -> [ Buff1 (dst, src, init) ]
   | Split (g, i, os) -> ( match g with | Bits gs -> ( match List.exists gs
   ~f:(Map.mem const_vars) with | false -> [ Split (g, i, os) ] | true -> ( (*
   first remove null branches *) let not_dead = List.zip_exn gs os |>
   List.filter ~f:(fun (g, _) -> let is_dead = Map.find const_vars g |>
   Option.map ~f:(CInt.eq CInt.zero) |> Option.value ~default:false in not
   is_dead) in let guarded_by_one = List.filter not_dead ~f:(fun (g, _) ->
   Map.find const_vars g |> Option.map ~f:(CInt.eq CInt.one) |> Option.value
   ~default:false) in let guarded_by_one = match guarded_by_one with | [] ->
   None | [ (_, o) ] -> Some o | _ -> failwith "More than one true guard" in
   match guarded_by_one with | Some o -> ( match o with | None -> [] | Some o ->
   [ MultiAssign [ (o, Var i) ] ]) | None -> let gs, os = List.unzip not_dead in
   [ Split (Bits gs, i, os) ])) | g -> [ Split (g, i, os) ]) | Merge (g, ins, o)
   -> [ Merge (g, ins, o) ]) in { Proc.stmt = dflows; iports; oports } *)

(* There a number of optimizations we can do if we have a pairs of split-merges
   with the same guard and some "parrelel" inputs/outputs *)
(* if we have the following structure v <- const split_g(v) -> (v1, v2, v3)
   merge_g(v1, v2', v3') -> v'

   We may rewrite it as v <- const split_g(v) -> (v1, v2, v3) v1' <- const
   merge_g(v1', v2', v3') -> v' *)
let push_consts_throguh_split_merge { Proc.stmt = dflows; iports; oports } =
  let map_parallel_split_merges dflows ~fuse =
    let split_merges, other_dflows =
      List.partition_map dflows ~f:(fun dflow ->
          match dflow with
          | Split (g, _, _) | Merge (g, _, _) -> Either.First (g, dflow)
          | _ -> Second dflow)
    in
    (* first bit things by guard varaible *)
    let split_merges =
      Guard.Map.of_alist_multi split_merges
      |> Map.data
      (* Now handle one guard cluster at a time *)
      |> List.concat_map ~f:(fun l ->
             let l = Array.of_list l in
             let idxs = List.init (Array.length l) ~f:Fn.id in
             let new_procs =
               List.concat_map idxs ~f:(fun i ->
                   List.concat_map idxs ~f:(fun j ->
                       match (l.(i), l.(j)) with
                       | Split (g, in_v, os), Merge (_, ins, out_v) -> (
                           match fuse in_v os ins with
                           | None -> []
                           | Some (os, ins, new_procs) ->
                               l.(i) <- Split (g, in_v, os);
                               l.(j) <- Merge (g, ins, out_v);
                               new_procs)
                       | _ -> []))
             in
             Array.to_list l @ new_procs)
    in

    split_merges @ other_dflows
  in
  let next_id =
    let biggest_id =
      var_ids dflows iports oports
      |> Set.to_list
      |> List.map ~f:(fun v -> v.id)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    ref (biggest_id + 1)
  in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let const_vars =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> (
            match FBlock.get_consts_if_const fblock with
            | None -> []
            | Some l -> l)
        | _ -> [])
    |> Var.Map.of_alist_exn
  in
  let dflows =
    map_parallel_split_merges dflows ~fuse:(fun in_v split_vars merge_vars ->
        let%bind.Option in_e = Map.find const_vars in_v in
        if not (Int.equal (List.length split_vars) (List.length merge_vars))
        then None
        else
          let merge_vars, new_procs =
            List.zip_exn split_vars merge_vars
            |> List.map ~f:(fun (split_var, merge_var) ->
                   if
                     Option.is_some split_var
                     && Option.value_exn split_var |> Var.equal merge_var
                   then
                     let new_v = new_chan in_v.bitwidth in
                     let new_proc =
                       [ MultiAssign (FBlock.create1 new_v (Const in_e)) ]
                     in
                     (new_v, new_proc)
                   else (merge_var, []))
            |> List.unzip
          in
          Some (split_vars, merge_vars, List.concat new_procs))
  in
  { Proc.stmt = dflows; iports; oports }

(* if we have the following structure split_g(v) -> (v1, v2, v3) merge_g(v1, v2,
   v3') -> v' and the merge is the only read of v1 and v2

   We may rewrite it as g' <- {g{0}|g{1}, g{2}} split_g(v) -> (v12, v3)
   merge_g(v12', v3') -> v' *)
let zip_coupled_split_merges { Proc.stmt = dflows; iports; oports } =
  let next_id =
    let biggest_id =
      var_ids dflows iports oports
      |> Set.to_list
      |> List.map ~f:(fun v -> v.id)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    ref (biggest_id + 1)
  in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let read_ct_of_id =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock ->
            FBlock.ins fblock |> Var.Set.of_list |> Set.to_list
        | Split (g, v, _) -> v :: Guard.ids g
        | Merge (g, ins, _) -> Guard.ids g @ ins
        | Buff1 (dst, src, _) -> [ dst; src ])
    |> List.map ~f:(fun v -> (v, ()))
    |> Var.Map.of_alist_multi |> Map.map ~f:List.length
  in

  let split_merges, other_dflows =
    List.partition_map dflows ~f:(fun dflow ->
        match dflow with
        | Split (_, _, _) | Merge (_, _, _) -> Either.First dflow
        | _ -> Second dflow)
  in
  (* first bit things by guard varaible *)
  let split_merges = List.mapi split_merges ~f:(fun i dflow -> (i, dflow)) in
  let couples =
    List.concat_map split_merges ~f:(fun (split_idx, split) ->
        List.concat_map split_merges ~f:(fun (merge_idx, merge) ->
            match (split, merge) with
            | Split (g_split, _, os), Merge (g_merge, ins, _) ->
                if Guard.equal g_split g_merge then
                  match g_split with
                  | Bits _ ->
                      if
                        (* lists are same length because guards are same and
                           both "bits" guards so same number of guards in
                           lists *)
                        List.zip_exn os ins
                        |> List.filter ~f:(fun (s, m) ->
                               Option.is_some s
                               && Var.equal (Option.value_exn s) m
                               && Map.find_exn read_ct_of_id
                                    (Option.value_exn s)
                                  |> Int.equal 1)
                        |> List.length >= 2
                      then [ (split_idx, merge_idx) ]
                      else []
                  | _ -> []
                else []
            | _ -> []))
  in
  (* then make sure this is no split/merge has multiple partners. TODO there are
     better ways to do this *)
  let couples =
    couples |> Int.Map.of_alist_multi |> Map.map ~f:List.hd_exn |> Map.to_alist
    |> List.map ~f:(fun (split, merge) -> (merge, split))
    |> Int.Map.of_alist_multi |> Map.map ~f:List.hd_exn |> Map.to_alist
    |> List.map ~f:(fun (merge, split) -> (split, merge))
  in

  let modify_set =
    List.concat_map couples ~f:(fun (split, merge) -> [ split; merge ])
    |> Int.Set.of_list
  in
  let unmod_split_merges, to_modify_split_merges =
    List.partition_map split_merges ~f:(fun (idx, dflow) ->
        if Set.mem modify_set idx then Either.Second (idx, dflow)
        else Either.First dflow)
  in
  let to_modify_split_merges = Int.Map.of_alist_exn to_modify_split_merges in
  let modified_split_merges =
    List.concat_map couples ~f:(fun (split, merge) ->
        match
          ( Map.find_exn to_modify_split_merges split,
            Map.find_exn to_modify_split_merges merge )
        with
        | Split (g, in_v, os), Merge (_, ins, out_v) -> (
            match g with
            | Bits gs ->
                let parrellel, not_parrellel =
                  List.zip_exn gs (List.zip_exn os ins)
                  |> List.partition_map ~f:(fun (g, (s, m)) ->
                         if
                           Option.is_some s
                           && Var.equal (Option.value_exn s) m
                           && Map.find_exn read_ct_of_id (Option.value_exn s)
                              |> Int.equal 1
                         then Either.First (g, s, m)
                         else Either.Second (g, s, m))
                in
                let parrellel_g = new_chan 1 in
                let parrellel_expr =
                  List.map parrellel ~f:(fun (g, _, _) -> F_expr.Var g)
                  |> List.reduce_exn ~f:(fun a b -> F_expr.BitOr (a, b))
                in
                let par_chan = new_chan in_v.bitwidth in
                let ctrl_proc =
                  MultiAssign (FBlock.create1 parrellel_g parrellel_expr)
                in
                let gs, ss, ms = List.unzip3 not_parrellel in
                let gs = gs @ [ parrellel_g ] in
                let ss = ss @ [ Some par_chan ] in
                let ms = ms @ [ par_chan ] in
                [
                  Split (Bits gs, in_v, ss);
                  Merge (Bits gs, ms, out_v);
                  ctrl_proc;
                ]
            | _ -> failwith "unreachable")
        | _ -> failwith "unreachable")
  in

  let dflows = unmod_split_merges @ modified_split_merges @ other_dflows in
  { Proc.stmt = dflows; iports; oports }

(* if we have the following structure split_g(v) -> ( *, *, v3 )

   We may rewrite it as g' <- {g{0}|g{1}, g{2}} split_g(v) -> ( *, v3 ) *)

(* if we have the following structure split_g(v) -> ( *, *, v3 )

   We may rewrite it as g' <- {g{0}|g{1}, g{2}} split_g(v) -> ( *, v3 ) *)

let zip_repeated_sink_splits { Proc.stmt = dflows; iports; oports } =
  let next_id =
    let biggest_id =
      var_ids dflows iports oports
      |> Set.to_list
      |> List.map ~f:(fun v -> v.id)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    ref (biggest_id + 1)
  in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let dflows =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | Split (g, i, os) -> (
            match g with
            | Bits gs ->
                let sinks, non_sinks =
                  List.zip_exn gs os
                  |> List.partition_tf ~f:(fun (_, o) -> Option.is_none o)
                in
                if List.is_empty non_sinks then []
                else if List.length sinks < 2 then [ dflow ]
                else
                  let gs, _ = List.unzip sinks in
                  let g = new_chan 1 in
                  let expr =
                    List.map gs ~f:(fun g -> F_expr.Var g)
                    |> List.reduce_exn ~f:(fun a b -> F_expr.BitOr (a, b))
                  in
                  let ctrl_proc = MultiAssign (FBlock.create1 g expr) in
                  let gs, os = List.unzip (non_sinks @ [ (g, None) ]) in
                  [ Split (Bits gs, i, os); ctrl_proc ]
            | _ -> (* TODO *) [ dflow ])
        | _ -> [ dflow ])
  in
  { Proc.stmt = dflows; iports; oports }

let zip_repeated_merge_consts { Proc.stmt = dflows; iports; oports } =
  let const_vars =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | MultiAssign fblock -> (
            match FBlock.get_consts_if_const fblock with
            | None -> []
            | Some l -> l)
        | _ -> [])
    |> Var.Map.of_alist_exn
  in

  let next_id =
    let biggest_id =
      var_ids dflows iports oports
      |> Set.to_list
      |> List.map ~f:(fun v -> v.id)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    ref (biggest_id + 1)
  in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let dflows =
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | Merge (g, ins, o) -> (
            match g with
            | Bits gs ->
                let const, non_const =
                  List.zip_exn gs ins
                  |> List.partition_map ~f:(fun (g, in_v) ->
                         match Map.find const_vars in_v with
                         | Some c -> Either.First (c, (g, in_v))
                         | None -> Either.Second (g, in_v))
                in

                let const, ctrl_procs =
                  CInt.Map.of_alist_multi const
                  |> Map.map ~f:(fun l ->
                         let gs, in_vs = List.unzip l in
                         let g, ctrl_proc =
                           match gs with
                           | [] -> failwith "unreachable"
                           | [ g ] -> (g, [])
                           | gs ->
                               let g = new_chan 1 in
                               let expr =
                                 List.map gs ~f:(fun g -> F_expr.Var g)
                                 |> List.reduce_exn ~f:(fun a b ->
                                        F_expr.BitOr (a, b))
                               in
                               let assign =
                                 MultiAssign (FBlock.create1 g expr)
                               in
                               (g, [ assign ])
                         in
                         ((g, List.hd_exn in_vs), ctrl_proc))
                  |> Map.data |> List.unzip
                in
                let gs, ins = List.unzip (non_const @ const) in
                Merge (Bits gs, ins, o) :: List.concat ctrl_procs
            | _ -> (* TODO *) [ dflow ])
        | _ -> [ dflow ])
  in
  { Proc.stmt = dflows; iports; oports }

let optimize_proc proc =
  let proc =
    eliminate_dead_code proc |> eliminate_repeated_vars
    (* in let _proc = proc *) |> eliminate_dead_code
    |> propogate_consts |> cluster_same_reads |> cluster_fuse_chains
    |> cluster_same_reads |> propogate_consts |> push_consts_throguh_split_merge
    |> eliminate_repeated_vars |> propogate_consts |> zip_coupled_split_merges
    |> zip_repeated_merge_consts |> propogate_consts |> zip_repeated_sink_splits
    |> eliminate_dead_code |> cluster_same_reads |> propogate_consts
    |> normalize_guards |> cluster_same_reads |> cluster_fuse_chains
    |> propogate_consts |> cluster_same_reads |> eliminate_repeated_vars
    |> eliminate_dead_code
  in
  validate proc;
  proc
