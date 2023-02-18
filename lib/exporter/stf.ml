open! Core

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

module Proc = struct
  type t = {
    stmt : Stmt.t;
    iports : (Interproc_chan.t * Chan.t) list;
    oports : (Interproc_chan.t * Chan.t) list;
  }
  [@@deriving sexp_of]
end

let stf_of_dflowable_chp_proc proc =
  assert proc.Flat_program.Chp.Proc.dflowable;
  (* then put the program in standard token form *)
  let of_c (c : Flat_program.Chp.Chan.t) =
    { Chan.id = Flat_program.Chp.Chan.Id.to_int c.id; bitwidth = c.bitwidth }
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
              { Var.id; bitwidth = v.Flat_program.Chp.Var.bitwidth })
    in
    let of_v v = of_v' v ~stf_id_of_id ~stf_id_of_raw_read_id in

    let write_v' v ~stf_id_of_id =
      let id = !next_v_id in
      incr next_v_id;
      let data = { Var.id; bitwidth = v.Flat_program.Chp.Var.bitwidth } in
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
    | Flat_program.Chp.Stmt.Nop -> Stmt.Nop
    | Log _ -> Nop
    | Assert _ -> Nop
    | Assign (_, v, e) -> Assign (write_v v, of_e e)
    | Send (_, c, e) -> Send (of_c c, of_e e)
    | ReadThenAssert (_, c, v, _) -> Read (of_c c, write_v v)
    | Seq stmts ->
        let stmts =
          List.map stmts ~f:(fun stmt ->
              of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id)
        in
        Seq stmts
    | Par stmts ->
        let l =
          List.map stmts ~f:(fun stmt ->
              let stf_id_of_id = Flat_program.Chp.Var.Table.create () in
              let stf_id_of_raw_read_id =
                Flat_program.Chp.Var.Table.create ()
              in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let stmts = List.map l ~f:(fun (stmt, _, _) -> stmt) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) ->
              Hashtbl.keys stf_id_of_id)
          |> Flat_program.Chp.Var.Set.of_list
        in
        let raw_read_ids =
          List.concat_map l ~f:(fun (_, _, stf_id_of_raw_read_id) ->
              Hashtbl.keys stf_id_of_raw_read_id)
          |> Flat_program.Chp.Var.Set.of_list
        in
        (* print_s [%sexp (("pre_split stf_id_of_raw_read_id", stf_id_of_raw_read_id): string * Var.t Var.Table.t )]; *)
        (* print_s [%sexp (("pre_split stf_id_of_id", stf_id_of_id): string * Var.t Var.Table.t)]; *)
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
        (* print_s [%sexp (("splits", raw_read_ids, splits): string * Var.Set.t * Stmt.Par_split.t list)]; *)
        (* print_s [%sexp (("merges", merges): string * Stmt.Par_merge.t list)]; *)
        Par (splits, stmts, merges)
    | SelectImm (_, gaurd_expr, branches) ->
        let gaurd_expr = of_e gaurd_expr in
        let l =
          List.map branches ~f:(fun stmt ->
              let stf_id_of_id = Flat_program.Chp.Var.Table.create () in
              let stf_id_of_raw_read_id =
                Flat_program.Chp.Var.Table.create ()
              in
              let stmt = of_stmt stmt ~stf_id_of_id ~stf_id_of_raw_read_id in
              (stmt, stf_id_of_id, stf_id_of_raw_read_id))
        in
        let branches = List.map l ~f:(fun (branch, _, _) -> branch) in
        let write_ids =
          List.concat_map l ~f:(fun (_, stf_id_of_id, _) ->
              Hashtbl.keys stf_id_of_id)
          |> Flat_program.Chp.Var.Set.of_list
        in
        (* every branch must either write the write_id or have it as a raw read. If not, add it as a raw read *)
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
          |> Flat_program.Chp.Var.Set.of_list
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
        SelectImm (gaurd_expr, splits, branches, merges)
    | DoWhile (_, stmt, guard) ->
        let stf_id_of_id' = Flat_program.Chp.Var.Table.create () in
        let stf_id_of_raw_read_id' = Flat_program.Chp.Var.Table.create () in
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
          |> Flat_program.Chp.Var.Set.of_list |> Set.to_list
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
  let stf_id_of_id = Flat_program.Chp.Var.Table.create () in
  let stf_id_of_raw_read_id = Flat_program.Chp.Var.Table.create () in
  let stmt =
    of_stmt proc.Flat_program.Chp.Proc.stmt ~stf_id_of_id ~stf_id_of_raw_read_id
  in
  assert (Hashtbl.is_empty stf_id_of_raw_read_id);
  let iports =
    List.map proc.iports ~f:(fun (interproc, chan) -> (interproc, of_c chan))
  in
  let oports =
    List.map proc.oports ~f:(fun (interproc, chan) -> (interproc, of_c chan))
  in
  { Proc.stmt; iports; oports }

let optimize_proc proc =
  (* flatten the generated code. TODO flatten par blocks *)
  let rec flatten n =
    match n with
    | Stmt.Nop -> Stmt.Nop
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
    | SelectImm (splits, gaurd_var, branches, merges) ->
        let branches = List.map branches ~f:flatten in
        SelectImm (splits, gaurd_var, branches, merges)
    | DoWhile (phis, stmt, guard) ->
        let stmt = flatten stmt in
        DoWhile (phis, stmt, guard)
  in

  let eliminate_dead_code n =
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
      Expr.var_ids e |> iter_any ~f:(fun id -> set_alive id ~vl)
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
      | SelectImm (guard, splits, ns, merges) ->
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
          let b4 = set_e_alive guard ~vl:true in
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
      | SelectImm (guard, splits, ns, merges) ->
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
          SelectImm (guard, splits, ns, merges)
      | DoWhile (phis, ns, guard) ->
          let phis =
            List.filter_map phis ~f:(fun phi ->
                let fo v =
                  Option.bind v ~f:(fun v ->
                      if is_alive v then Some v else None)
                in
                let init_v = fo phi.init_v in
                let body_in_v = fo phi.body_in_v in
                let body_out_v = fo phi.body_out_v in
                let out_v = fo phi.out_v in
                match (init_v, body_in_v, body_out_v, out_v) with
                | None, None, None, None -> None
                | _, _, _, _ ->
                    Some { DoWhile_phi.init_v; body_in_v; body_out_v; out_v })
          in
          let ns = of_n ns in
          DoWhile (phis, ns, guard)
    in
    of_n n
  in

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
          | _ -> Assign (dst, Expr.map_vars e ~f:of_v))
      | Send (chan, e) -> Send (chan, Expr.map_vars e ~f:of_v)
      | Read (chan, v) -> Read (chan, v)
      | Seq ns -> Seq (List.map ns ~f:of_n)
      | Par (splits, ns, merges) ->
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
      | SelectImm (guard, splits, ns, merges) ->
          (* TODO handle repeated splits/merges for same varaible *)
          let guard = Expr.map_vars guard ~f:of_v in
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
          SelectImm (guard, splits, ns, merges)
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
          let guard = Expr.map_vars guard ~f:of_v in
          DoWhile (phis, ns, guard)
    in
    of_n n
  in

  let stmt =
    flatten proc.Proc.stmt |> eliminate_dead_code |> flatten
    |> eliminate_doubled_vars |> flatten |> eliminate_dead_code |> flatten
  in
  { Proc.stmt; iports = proc.iports; oports = proc.oports }
