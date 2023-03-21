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

module Chan = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

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

module Stmt = struct
  type t =
    | Nop
    | Assign of Var.t * Var.t F_expr.t
    | Read of Chan.t * Var.t
    | Send of Chan.t * Var.t F_expr.t
    | Seq of t list
    | Par of Par_split.t list * t list * Par_merge.t list
    | SelectImm of
        Var.t F_expr.t list * Select_split.t list * t list * Select_merge.t list
    | DoWhile of DoWhile_phi.t list * t * Var.t F_expr.t
  [@@deriving sexp]
end

module Proc = struct
  type t = {
    stmt : Stmt.t;
    iports : (Interproc_chan.t * Chan.t) list;
    oports : (Interproc_chan.t * Chan.t) list;
  }
  [@@deriving sexp_of]
end

let stf_of_dflowable_chp_proc proc =
  assert proc.Flat_chp.Proc.dflowable;
  (* then put the program in standard token form *)
  let of_c (c : Flat_chp.Chan.t) =
    { Chan.id = Flat_chp.Chan.Id.to_int c.id; bitwidth = c.bitwidth }
  in

  let next_v_id = ref 0 in
  let rec of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id =
    let of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id =
      match Hashtbl.find stf_id_of_id v with
      | Some id -> id
      | None ->
          Hashtbl.find_or_add stf_id_of_raw_read_id v ~default:(fun () ->
              let id = !next_v_id in
              incr next_v_id;
              { Var.id; bitwidth = v.Flat_chp.Var.bitwidth })
    in
    let of_v v = of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id in

    let write_v' v ~stf_id_of_id =
      let id = !next_v_id in
      incr next_v_id;
      let data = { Var.id; bitwidth = v.Flat_chp.Var.bitwidth } in
      Hashtbl.set stf_id_of_id ~key:v ~data;
      data
    in
    let write_v v = write_v' v ~stf_id_of_id in

    let of_e' e ~stf_id_of_id ~stf_id_of_raw_read_id =
      F_expr.map_vars e ~f:(fun v -> of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id)
    in
    let of_e e = of_e' e ~stf_id_of_id ~stf_id_of_raw_read_id in

    match stmt with
    | Flat_chp.Stmt.Nop -> Stmt.Nop
    | Assert _ -> Nop
    | Assign (v, e) -> Assign (write_v v, of_e e)
    | Send (c, e) -> Send (of_c c, of_e e)
    | ReadThenAssert (c, v, _) -> Read (of_c c, write_v v)
    | Seq stmts ->
        let stmts =
          List.map stmts ~f:(fun stmt ->
              of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id)
        in
        Seq stmts
    | Par stmts ->
        let l =
          List.map stmts ~f:(fun stmt ->
              let stf_id_of_id = Flat_chp.Var.Table.create () in
              let stf_id_of_raw_read_id = Flat_chp.Var.Table.create () in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let stmts = List.map l ~f:(fun (stmt, _, _) -> stmt) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) ->
              Hashtbl.keys stf_id_of_id)
          |> Flat_chp.Var.Set.of_list
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
              Hashtbl.keys stf_id_of_raw_read_id)
          |> Flat_chp.Var.Set.of_list
        in
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
                       Hashtbl.find stf_id_of_raw_read_id raw_read_id)
                 in
                 { Par_split.in_v; out_vs })
        in
        let merges =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l ~f:(fun (_, stf_id_of_id, _) ->
                       Hashtbl.find stf_id_of_id write_id)
                 in
                 let out_v = write_v write_id in
                 { Par_merge.in_vs; out_v })
        in
        Par (splits, stmts, merges)
    | Nondeterm_select _ -> failwith "STF does not support Nondeterm_select"
    | SelectImm (gaurds, branches) ->
        let gaurds = List.map gaurds ~f:of_e in
        let l =
          List.map branches ~f:(fun stmt ->
              let stf_id_of_id = Flat_chp.Var.Table.create () in
              let stf_id_of_raw_read_id = Flat_chp.Var.Table.create () in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let branches = List.map l ~f:(fun (branch, _, _) -> branch) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) ->
              Hashtbl.keys stf_id_of_id)
          |> Flat_chp.Var.Set.of_list
        in
        (* every branch must either write the write_id or have it as a raw read.
           If not, add it as a raw read *)
        let merges' =
          Set.to_list write_ids
          |> List.map ~f:(fun write_id ->
                 let in_vs =
                   List.map l
                     ~f:(fun (_, stf_id_of_id, stf_id_of_raw_read_id) ->
                       of_v' write_id ~stf_id_of_id ~stf_id_of_raw_read_id)
                 in
                 (write_id, in_vs))
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
              Hashtbl.keys stf_id_of_raw_read_id)
          |> Flat_chp.Var.Set.of_list
        in
        let splits =
          Set.to_list raw_read_ids
          |> List.map ~f:(fun raw_read_id ->
                 let in_v = of_v raw_read_id in
                 let out_vs =
                   List.map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
                       Hashtbl.find stf_id_of_raw_read_id raw_read_id)
                 in
                 { Select_split.in_v; out_vs })
        in
        let merges =
          List.map merges' ~f:(fun (write_id, in_vs) ->
              let out_v = write_v write_id in
              { Select_merge.in_vs; out_v })
        in
        SelectImm (gaurds, splits, branches, merges)
    | DoWhile (stmt, guard) ->
        let stf_id_of_id' = Flat_chp.Var.Table.create () in
        let stf_id_of_raw_read_id' = Flat_chp.Var.Table.create () in
        let stmt =
          of_stmt stmt ~stf_id_of_id:stf_id_of_id'
            ~stf_id_of_raw_read_id:stf_id_of_raw_read_id'
        in
        let guard =
          of_e' guard ~stf_id_of_id:stf_id_of_id'
            ~stf_id_of_raw_read_id:stf_id_of_raw_read_id'
        in
        let phis =
          Hashtbl.keys stf_id_of_id' @ Hashtbl.keys stf_id_of_raw_read_id'
          |> Flat_chp.Var.Set.of_list |> Set.to_list
          |> List.map ~f:(fun var_id ->
                 let body_read = Hashtbl.find stf_id_of_raw_read_id' var_id in
                 let body_write = Hashtbl.find stf_id_of_id' var_id in
                 match (body_read, body_write) with
                 | None, None -> failwith "unreachable"
                 | Some body_read, None ->
                     let init_v = of_v var_id in
                     {
                       DoWhile_phi.init_v = Some init_v;
                       body_in_v = Some body_read;
                       body_out_v = Some body_read;
                       out_v = None;
                     }
                 | None, Some body_write ->
                     (* let init_v = of_v var_id in *)
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
  let stf_id_of_id = Flat_chp.Var.Table.create () in
  let stf_id_of_raw_read_id = Flat_chp.Var.Table.create () in
  let stmt =
    of_stmt proc.Flat_chp.Proc.stmt ~stf_id_of_id ~stf_id_of_raw_read_id
  in
  assert (Hashtbl.is_empty stf_id_of_raw_read_id);
  let iports =
    List.map proc.iports ~f:(fun (interproc, chan) -> (interproc, of_c chan))
  in
  let oports =
    List.map proc.oports ~f:(fun (interproc, chan) -> (interproc, of_c chan))
  in
  { Proc.stmt; iports; oports }

let flatten n =
  (* TODO combine these functions? *)
  let rec does_chan_io n =
    match n with
    | Stmt.Nop -> false
    | Assign _ -> false
    | Send _ -> true
    | Read _ -> true
    | Seq stmts -> List.exists stmts ~f:does_chan_io
    | Par (_, stmts, _) -> List.exists stmts ~f:does_chan_io
    | SelectImm (_, _, branches, _) -> List.exists branches ~f:does_chan_io
    | DoWhile (_, stmt, _) -> does_chan_io stmt
  in

  let rec flatten n =
    let flatten_seqs stmts =
      let stmts =
        List.concat_map stmts ~f:(fun stmt ->
            match stmt with
            | Stmt.Nop -> []
            | Seq stmts -> stmts
            | _ -> [ stmt ])
      in
      match stmts with [] -> Stmt.Nop | [ stmt ] -> stmt | stmts -> Seq stmts
    in
    match n with
    | Stmt.Nop -> Stmt.Nop
    | Assign (v, e) -> Assign (v, e)
    | Send (c, e) -> Send (c, e)
    | Read (c, v) -> Read (c, v)
    | Seq stmts -> List.map stmts ~f:flatten |> flatten_seqs
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
                         Stmt.Assign (out_v, Var split.in_v)))
            in
            let copy_outs =
              List.filter_map merges ~f:(fun merge ->
                  assert (Int.equal 1 (List.length merge.in_vs));
                  List.hd_exn merge.in_vs
                  |> Option.map ~f:(fun in_v ->
                         Stmt.Assign (merge.out_v, Var in_v)))
            in
            Seq (copy_ins @ [ stmt ] @ copy_outs)
        | stmts ->
            let stmts = List.map stmts ~f:flatten in
            (* TODO flatten nested par statements *)
            Par (splits, stmts, merges))
    | SelectImm (gaurds, splits, branches, merges) -> (
        let branches = List.map branches ~f:flatten in
        if List.is_empty merges && not (List.exists ~f:does_chan_io branches)
        then Nop
        else
          match branches with
          | [] -> failwith "Undefined behavior in user code"
          | [ branch ] ->
              let prolog =
                List.filter_map splits ~f:(fun split ->
                    match split.out_vs with
                    | [ None ] -> None
                    | [ Some out_v ] ->
                        (* Let eliminate_doubled_vars take care of these *)
                        Some (Stmt.Assign (out_v, Var split.in_v))
                    | _ -> failwith "unreachable")
              in
              let postlog =
                List.map merges ~f:(fun merge ->
                    match merge.in_vs with
                    | [ in_v ] ->
                        (* Let eliminate_doubled_vars take care of these *)
                        Stmt.Assign (merge.out_v, Var in_v)
                    | _ -> failwith "unreachable")
              in
              flatten_seqs (prolog @ [ branch ] @ postlog)
          | branches ->
              (* TODO flatten out Nop branches *)
              SelectImm (gaurds, splits, branches, merges))
    | DoWhile (phis, stmt, guard) ->
        let stmt = flatten stmt in
        if
          (not (List.exists phis ~f:(fun phi -> Option.is_some phi.out_v)))
          && not (does_chan_io stmt)
        then Nop
        else (* TODO flatten out Nop branches *)
          DoWhile (phis, stmt, guard)
  in
  flatten n

let eliminate_dead_variables n =
  let any l = List.exists l ~f:Fn.id in
  let iter_any l ~f = List.map l ~f |> any in
  let map_or_false v ~f = match v with Some v -> f v | None -> false in
  let table = Var.Table.create () in
  let is_alive v = Hashtbl.find_or_add table v ~default:(fun () -> false) in
  let set_alive v = Hashtbl.set table ~key:v ~data:true in
  let set_alive v ~vl =
    if vl then (
      let b = not (is_alive v) in
      set_alive v;
      b)
    else false
  in
  let set_e_alive e ~vl =
    F_expr.var_ids e |> iter_any ~f:(fun id -> set_alive id ~vl)
  in

  let is_alive_o v = map_or_false v ~f:is_alive in
  let set_alive_o v ~vl = map_or_false v ~f:(set_alive ~vl) in

  let rec stabilize_stmt n =
    match n with
    | Stmt.Nop -> false
    | Assign (dst, e) -> set_e_alive e ~vl:(is_alive dst)
    | Send (_, e) -> set_e_alive e ~vl:true
    | Read (_, _) -> false
    | Seq ns -> List.rev ns |> iter_any ~f:stabilize_stmt
    | Par (splits, ns, merges) ->
        let b1 =
          iter_any merges ~f:(fun merge ->
              iter_any merge.in_vs ~f:(set_alive_o ~vl:(is_alive merge.out_v)))
        in
        let b2 = iter_any ns ~f:stabilize_stmt in
        let b3 =
          iter_any splits ~f:(fun split ->
              let any_alive = List.exists split.out_vs ~f:is_alive_o in
              set_alive split.in_v ~vl:any_alive)
        in
        any [ b1; b2; b3 ]
    | SelectImm (guards, splits, ns, merges) ->
        let b1 =
          iter_any merges ~f:(fun merge ->
              iter_any merge.in_vs ~f:(set_alive ~vl:(is_alive merge.out_v)))
        in
        let b2 = iter_any ns ~f:stabilize_stmt in
        let b3 =
          iter_any splits ~f:(fun split ->
              let any_alive = List.exists split.out_vs ~f:is_alive_o in
              set_alive split.in_v ~vl:any_alive)
        in
        let b4 = iter_any guards ~f:(fun guard -> set_e_alive guard ~vl:true) in
        any [ b1; b2; b3; b4 ]
    | DoWhile (phis, ns, guard) ->
        let changed =
          let b1 =
            iter_any phis ~f:(fun phi ->
                map_or_false phi.out_v ~f:(fun out_v ->
                    set_alive
                      (Option.value_exn phi.body_out_v)
                      ~vl:(is_alive out_v)))
          in
          let b2 = set_e_alive guard ~vl:true in
          ref (any [ b1; b2 ])
        in

        while
          let b1 = stabilize_stmt ns in
          let b2 =
            iter_any phis ~f:(fun phi ->
                map_or_false phi.body_in_v ~f:(fun body_in_v ->
                    iter_any
                      [ phi.body_out_v; phi.init_v ]
                      ~f:(set_alive_o ~vl:(is_alive body_in_v))))
          in

          any [ b1; b2 ]
        do
          changed := true
        done;
        !changed
  in

  let (_ : bool) = stabilize_stmt n in
  let changed = stabilize_stmt n in
  assert (not changed);
  let rec of_n n =
    match n with
    | Stmt.Nop -> Stmt.Nop
    | Assign (dst, e) -> if not (is_alive dst) then Nop else Assign (dst, e)
    | Send (chan, e) -> Send (chan, e)
    | Read (chan, v) -> Read (chan, v)
    | Seq ns -> Seq (List.map ns ~f:of_n)
    | Par (splits, ns, merges) ->
        let splits =
          List.filter_map splits ~f:(fun split ->
              let out_vs =
                List.map split.out_vs
                  ~f:
                    (Option.bind ~f:(fun out ->
                         if is_alive out then Some out else None))
              in
              if List.exists out_vs ~f:Option.is_some then
                Some { Par_split.in_v = split.in_v; out_vs }
              else None)
        in
        let ns = List.map ns ~f:of_n in
        let merges =
          List.filter_map merges ~f:(fun merge ->
              if is_alive merge.out_v then Some merge else None)
        in
        Par (splits, ns, merges)
    | SelectImm (guards, splits, ns, merges) ->
        let splits =
          List.filter_map splits ~f:(fun split ->
              let out_vs =
                List.map split.out_vs
                  ~f:
                    (Option.bind ~f:(fun out ->
                         if is_alive out then Some out else None))
              in
              if List.exists out_vs ~f:Option.is_some then
                Some { Select_split.in_v = split.in_v; out_vs }
              else None)
        in
        let ns = List.map ns ~f:of_n in
        let merges =
          List.filter_map merges ~f:(fun merge ->
              if is_alive merge.out_v then Some merge else None)
        in
        SelectImm (guards, splits, ns, merges)
    | DoWhile (phis, ns, guard) ->
        let phis =
          List.filter_map phis ~f:(fun phi ->
              let fo v =
                Option.bind v ~f:(fun v -> if is_alive v then Some v else None)
              in
              let body_in_v = fo phi.body_in_v in
              let out_v = fo phi.out_v in
              let init_v =
                if Option.is_none body_in_v then None else phi.init_v
              in
              let body_out_v =
                if Option.is_none body_in_v && Option.is_none out_v then None
                else phi.body_out_v
              in
              match (init_v, body_in_v, body_out_v, out_v) with
              | None, None, None, None -> None
              | _, _, _, _ ->
                  Some { DoWhile_phi.init_v; body_in_v; body_out_v; out_v })
        in
        let ns = of_n ns in
        DoWhile (phis, ns, guard)
  in
  of_n n

let eliminate_doubled_vars n =
  let renames = Var.Table.create () in
  let rec of_n n =
    let of_v v = Hashtbl.find renames v |> Option.value ~default:v in
    match n with
    | Stmt.Nop -> Stmt.Nop
    | Assign (dst, e) -> (
        match e with
        | Var v ->
            Hashtbl.set renames ~key:dst ~data:(of_v v);
            Nop
        | _ -> Assign (dst, F_expr.map_vars e ~f:of_v))
    | Send (chan, e) -> Send (chan, F_expr.map_vars e ~f:of_v)
    | Read (chan, v) -> Read (chan, v)
    | Seq ns -> Seq (List.map ns ~f:of_n)
    | Par (splits, ns, merges) ->
        (* any split that passes directly to a merge is a repeated var *)
        List.iter merges ~f:(fun merge ->
            List.filter_opt merge.in_vs
            |> List.iter ~f:(fun in_v ->
                   List.iter splits ~f:(fun split ->
                       List.iter split.out_vs ~f:(fun out_v ->
                           if Option.equal Var.equal (Some in_v) out_v then
                             Hashtbl.set renames ~key:merge.out_v
                               ~data:(of_v split.in_v)))));

        (* TODO handle repeated splits/merges for same varaible *)
        let splits =
          List.map splits ~f:(fun split ->
              let in_v = of_v split.in_v in
              { Par_split.in_v; out_vs = split.out_vs })
        in
        let ns = List.map ns ~f:of_n in
        let merges =
          List.map merges ~f:(fun merge ->
              let in_vs = List.map merge.in_vs ~f:(Option.map ~f:of_v) in
              { Par_merge.in_vs; out_v = merge.out_v })
        in
        Par (splits, ns, merges)
    | SelectImm (guards, splits, ns, merges) ->
        (* TODO handle repeated splits/merges for same varaible *)
        let guards = List.map guards ~f:(F_expr.map_vars ~f:of_v) in
        let splits =
          List.map splits ~f:(fun split ->
              let in_v = of_v split.in_v in
              { Select_split.in_v; out_vs = split.out_vs })
        in
        let ns = List.map ns ~f:of_n in
        let merges =
          List.map merges ~f:(fun merge ->
              let in_vs = List.map merge.in_vs ~f:of_v in
              { Select_merge.in_vs; out_v = merge.out_v })
        in
        SelectImm (guards, splits, ns, merges)
    | DoWhile (phis, ns, guard) ->
        let phis =
          List.map phis ~f:(fun phi ->
              {
                DoWhile_phi.init_v = Option.map ~f:of_v phi.init_v;
                body_in_v = phi.body_in_v;
                body_out_v = Option.map phi.body_out_v ~f:of_v;
                out_v = phi.out_v;
              })
        in
        let ns = of_n ns in
        let guard = F_expr.map_vars guard ~f:of_v in
        DoWhile (phis, ns, guard)
  in
  of_n n

let propigate_constants ?(max_ct = 16) n =
  let module Lat = Cint_value_lattice in
  let any l = List.exists l ~f:Fn.id in
  let iter_any l ~f = List.map l ~f |> any in
  let map_or_false v ~f = match v with Some v -> f v | None -> false in

  (* include a count on each updated varaible. Otherwise, this may take a huge
     number of iterations *)
  let table = Var.Table.create () in
  let get v = Hashtbl.find_exn table v |> fst in
  let put v new_vl =
    match Hashtbl.find table v with
    | Some (prev, ct) ->
        let new_vl, new_ct =
          if ct < max_ct then
            let new_vl = Lat.union prev new_vl in
            (new_vl, ct + 1)
          else (Lat.create_bitwidth v.bitwidth, max_ct)
        in
        Hashtbl.set table ~key:v ~data:(new_vl, new_ct);
        not (Lat.equal prev new_vl)
    | None ->
        Hashtbl.set table ~key:v ~data:(new_vl, 0);
        true
  in
  let expr_val e = Lat.eval_expr e ~of_var:get in

  let rec stabilize_stmt n =
    match n with
    | Stmt.Nop -> false
    | Assign (dst, e) -> put dst (expr_val e)
    | Send (_, _) -> false
    | Read (_, v) -> put v (Lat.create_bitwidth v.bitwidth)
    | Seq ns -> iter_any ns ~f:stabilize_stmt
    | Par (splits, ns, merges) ->
        let b1 =
          iter_any splits ~f:(fun split ->
              let in_v_lat = get split.in_v in
              iter_any split.out_vs
                ~f:(map_or_false ~f:(fun v -> put v in_v_lat)))
        in
        let b2 = iter_any ns ~f:stabilize_stmt in
        let b3 =
          iter_any merges ~f:(fun merge ->
              let out_v_lat =
                List.filter_opt merge.in_vs
                |> List.map ~f:get |> List.reduce ~f:Lat.union
                |> Option.value_exn
              in
              put merge.out_v out_v_lat)
        in
        any [ b1; b2; b3 ]
    | SelectImm (_, splits, ns, merges) ->
        let b1 =
          iter_any splits ~f:(fun split ->
              let in_v_lat = get split.in_v in
              iter_any split.out_vs
                ~f:(map_or_false ~f:(fun v -> put v in_v_lat)))
        in
        let b2 = iter_any ns ~f:stabilize_stmt in
        let b3 =
          iter_any merges ~f:(fun merge ->
              let out_v_lat =
                List.map merge.in_vs ~f:get
                |> List.reduce ~f:Lat.union |> Option.value_exn
              in
              put merge.out_v out_v_lat)
        in
        any [ b1; b2; b3 ]
    | DoWhile (phis, ns, _) ->
        let changed =
          let b1 =
            iter_any phis ~f:(fun phi ->
                map_or_false phi.init_v ~f:(fun init_v ->
                    put (Option.value_exn phi.body_in_v) (get init_v)))
          in
          ref b1
        in

        while
          let b1 = stabilize_stmt ns in
          let b2 =
            iter_any phis ~f:(fun phi ->
                map_or_false phi.body_out_v ~f:(fun body_out_v ->
                    iter_any
                      [ phi.body_in_v; phi.out_v ]
                      ~f:(map_or_false ~f:(fun v -> put v (get body_out_v)))))
          in
          any [ b1; b2 ]
        do
          changed := true
        done;
        !changed
  in

  let (_ : bool) = stabilize_stmt n in
  let changed = stabilize_stmt n in
  assert (not changed);

  let of_e e = Lat.rewrite_expr e ~of_var:get in

  let rec of_n n =
    match n with
    | Stmt.Nop -> Stmt.Nop
    | Assign (dst, e) -> Assign (dst, of_e e)
    | Send (chan, e) -> Send (chan, of_e e)
    | Read (chan, v) -> Read (chan, v)
    | Seq ns -> Seq (List.map ns ~f:of_n)
    | Par (splits, ns, merges) -> Par (splits, List.map ns ~f:of_n, merges)
    | SelectImm (guards, splits, ns, merges) ->
        let ns = List.map ns ~f:of_n in
        let guards = List.map ~f:of_e guards in
        let is_true_branch =
          List.exists guards ~f:(fun e ->
              match e with Const c -> CInt.eq c CInt.one | _ -> false)
        in
        let is_false_branch =
          List.exists guards ~f:(fun e ->
              match e with Const c -> CInt.eq c CInt.zero | _ -> false)
        in
        let guards, splits, ns, merges =
          if (is_true_branch && List.length guards > 1) || is_false_branch then
            let to_drop =
              List.map guards ~f:(fun g ->
                  match g with
                  | Const c -> CInt.eq c CInt.one
                  | _ -> not is_true_branch)
            in
            let drop_l l =
              List.zip_exn l to_drop |> List.filter ~f:snd |> List.map ~f:fst
            in
            let splits =
              List.map splits ~f:(fun split ->
                  {
                    Select_split.in_v = split.in_v;
                    out_vs = drop_l split.out_vs;
                  })
            in
            let merges =
              List.map merges ~f:(fun merge ->
                  {
                    Select_merge.in_vs = drop_l merge.in_vs;
                    out_v = merge.out_v;
                  })
            in
            (drop_l guards, splits, drop_l ns, merges)
          else (guards, splits, ns, merges)
        in
        SelectImm (guards, splits, ns, merges)
    | DoWhile (phis, ns, guard) -> DoWhile (phis, of_n ns, of_e guard)
  in
  of_n n

let validate n =
  let assigned_ids = Var.Table.create () in
  let add id =
    assert (not (Hashtbl.mem assigned_ids id));
    Hashtbl.set assigned_ids ~key:id ~data:()
  in
  let rec of_n n =
    match n with
    | Stmt.Nop -> ()
    | Assign (dst, _) -> add dst
    | Send (_, _) -> ()
    | Read (_, v) -> add v
    | Seq ns -> List.iter ns ~f:of_n
    | Par (splits, ns, merges) ->
        List.iter splits ~f:(fun split ->
            List.iter split.out_vs ~f:(Option.iter ~f:add));

        List.iter ns ~f:of_n;
        List.iter merges ~f:(fun merge -> add merge.out_v)
    | SelectImm (_, splits, ns, merges) ->
        List.iter splits ~f:(fun split ->
            List.iter split.out_vs ~f:(Option.iter ~f:add));
        List.iter ns ~f:of_n;
        List.iter merges ~f:(fun merge -> add merge.out_v)
    | DoWhile (phis, ns, _) ->
        List.iter phis ~f:(fun phi ->
            Option.iter phi.body_in_v ~f:add;
            Option.iter phi.out_v ~f:add);
        of_n ns
  in
  of_n n

let optimize_proc proc =
  let stmt =
    flatten proc.Proc.stmt |> eliminate_dead_variables |> flatten
    |> eliminate_doubled_vars |> flatten |> eliminate_dead_variables |> flatten
    |> propigate_constants |> eliminate_dead_variables |> flatten
    |> eliminate_doubled_vars |> flatten |> eliminate_dead_variables |> flatten
  in
  validate stmt;
  { Proc.stmt; iports = proc.iports; oports = proc.oports }
