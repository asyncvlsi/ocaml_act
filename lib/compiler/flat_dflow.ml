open! Core
module CInt = Act.CInt

module Var = struct
  module T = struct
    type t = { id : int; bitwidth : int }
    [@@deriving sexp, hash, equal, compare]
  end

  include Comparable.Make (T)
  include Hashable.Make (T)
  include T
end

module FBlock = Fblock.Make (Var)

module Stmt = struct
  type t =
    | MultiAssign of FBlock.t
    | Split of Var.t * Var.t * Var.t option list
    | Merge of Var.t * Var.t list * Var.t
    | Copy_init of (*dst *) Var.t * (*src*) Var.t * Act.CInt.t
    | Clone of Var.t * Var.t list
    | Sink of Var.t
  [@@deriving sexp_of]
end

module Proc = struct
  type t = {
    stmt : Stmt.t list;
    iports : (Interproc_chan.t * Var.t) list;
    oports : (Interproc_chan.t * Var.t) list;
  }
  [@@deriving sexp_of]
end

let of_dflow_ir { Dflow_ir.Proc.stmt = dflows; iports; oports } =
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let reader_table = Dflow_ir.Var.Table.create () in
  let writer_table = Dflow_ir.Var.Table.create () in
  let of_v_read (v : Dflow_ir.Var.t) =
    let vv = new_chan v.bitwidth in
    Hashtbl.add_multi reader_table ~key:v ~data:vv;
    vv
  in
  let of_v_write (v : Dflow_ir.Var.t) =
    let vv = new_chan v.bitwidth in
    Hashtbl.add_exn writer_table ~key:v ~data:vv;
    vv
  in

  let dflows =
    List.map dflows ~f:(fun dflow ->
        match dflow with
        | Dflow_ir.Stmt.MultiAssign fblock ->
            let fblock =
              FBlock.map_ins_and_outs fblock
                ~map_in:(fun i -> of_v_read i)
                ~map_out:(fun i -> of_v_write i)
            in
            Stmt.MultiAssign fblock
        | Split (g, i, os) -> (
            match g with
            | Idx g ->
                Split
                  ( of_v_read g,
                    of_v_read i,
                    List.map os ~f:(Option.map ~f:of_v_write) )
            | _ -> failwith "TODO")
        | Merge (g, ins, o) -> (
            match g with
            | Idx g ->
                Merge (of_v_read g, List.map ins ~f:of_v_read, of_v_write o)
            | _ -> failwith "TODO")
        | Copy_init (dst, src, init) ->
            Copy_init (of_v_write dst, of_v_read src, init))
  in
  let iports = List.map iports ~f:(fun (x, i) -> (x, of_v_write i)) in
  let oports = List.map oports ~f:(fun (x, o) -> (x, of_v_read o)) in

  let reader_table =
    Hashtbl.to_alist reader_table |> Dflow_ir.Var.Map.of_alist_exn
  in
  let writer_table =
    Hashtbl.to_alist writer_table |> Dflow_ir.Var.Map.of_alist_exn
  in
  assert (
    Set.diff (Map.key_set reader_table) (Map.key_set writer_table)
    |> Set.is_empty);

  let readers_of_writer =
    Map.mapi writer_table ~f:(fun ~key ~data ->
        (data, Map.find_multi reader_table key))
    |> Map.data |> Var.Map.of_alist_exn
  in
  let copys_and_sinks =
    Map.to_alist readers_of_writer
    |> List.map ~f:(fun (writer, readers) ->
           match readers with
           | [] -> Stmt.Sink writer
           | readers -> Clone (writer, readers))
  in
  { Proc.stmt = dflows @ copys_and_sinks; iports; oports }

let flatten_copies { Proc.stmt = dflows; iports; oports } =
  (* For now this just involves removing copy nodes with a sing output *)
  let renames =
    List.filter_map dflows ~f:(fun dflow ->
        match dflow with Clone (src, [ dst ]) -> Some (dst, src) | _ -> None)
    |> Var.Map.of_alist_exn
  in
  let of_v v = Map.find renames v |> Option.value ~default:v in
  let dflows =
    List.filter_map dflows ~f:(fun dflow ->
        match dflow with
        | Stmt.MultiAssign fblock ->
            let fblock =
              FBlock.map_ins_and_outs fblock
                ~map_in:(fun i -> of_v i)
                ~map_out:(fun i -> of_v i)
            in
            Some (Stmt.MultiAssign fblock)
        | Split (g, i, os) ->
            Some (Split (of_v g, of_v i, List.map os ~f:(Option.map ~f:of_v)))
        | Merge (g, ins, o) ->
            Some (Merge (of_v g, List.map ins ~f:of_v, of_v o))
        | Copy_init (dst, src, init) ->
            Some (Copy_init (of_v dst, of_v src, init))
        | Clone (src, dsts) -> (
            match dsts with
            | [] -> failwith "Invalid copy"
            | [ _ ] -> None
            | dsts -> Some (Clone (of_v src, List.map dsts ~f:of_v)))
        | Sink v -> Some (Sink (of_v v)))
  in
  let iports = List.map iports ~f:(fun (x, i) -> (x, of_v i)) in
  let oports = List.map oports ~f:(fun (x, o) -> (x, of_v o)) in
  { Proc.stmt = dflows; iports; oports }

let var_ids { Proc.stmt = dflows; iports; oports } =
  [
    List.concat_map dflows ~f:(fun dflow ->
        match dflow with
        | Stmt.MultiAssign fblock -> FBlock.ins fblock @ FBlock.outs fblock
        | Split (g, i, os) -> g :: i :: List.filter_opt os
        | Merge (g, ins, o) -> g :: o :: ins
        | Copy_init (dst, src, _) -> [ dst; src ]
        | Clone (src, dsts) -> src :: dsts
        | Sink v -> [ v ]);
    List.map iports ~f:snd;
    List.map oports ~f:snd;
  ]
  |> List.concat |> Var.Set.of_list

let pack_var_names proc =
  (* Make all the variable names sequential *)
  let next_id = ref 0 in
  let new_chan bitwidth =
    let id = !next_id in
    incr next_id;
    { Var.id; bitwidth }
  in

  let var_ids =
    var_ids proc |> Map.of_key_set ~f:(fun id -> new_chan id.bitwidth)
  in
  let of_v v = Map.find_exn var_ids v in
  let dflows =
    List.map proc.stmt ~f:(fun dflow ->
        match dflow with
        | Stmt.MultiAssign fblock ->
            let fblock =
              FBlock.map_ins_and_outs fblock
                ~map_in:(fun i -> of_v i)
                ~map_out:(fun i -> of_v i)
            in
            Stmt.MultiAssign fblock
        | Split (g, i, os) ->
            Split (of_v g, of_v i, List.map os ~f:(Option.map ~f:of_v))
        | Merge (g, ins, o) -> Merge (of_v g, List.map ins ~f:of_v, of_v o)
        | Copy_init (dst, src, init) -> Copy_init (of_v dst, of_v src, init)
        | Clone (src, dsts) -> Clone (of_v src, List.map dsts ~f:of_v)
        | Sink v -> Sink (of_v v))
  in
  let iports = List.map proc.iports ~f:(fun (x, i) -> (x, of_v i)) in
  let oports = List.map proc.oports ~f:(fun (x, o) -> (x, of_v o)) in
  { Proc.stmt = dflows; iports; oports }

let optimize_proc proc = flatten_copies proc |> pack_var_names

let of_chp (chp : Flat_chp.Proc.t) =
  assert chp.dflowable;
  Stf.stf_of_dflowable_chp_proc chp
  |> Stf.optimize_proc |> Dflow_ir.dflow_of_stf |> Dflow_ir.optimize_proc
  |> of_dflow_ir |> optimize_proc
