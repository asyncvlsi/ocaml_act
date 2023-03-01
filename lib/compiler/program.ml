open! Core
module CInt = Act.CInt

module Mem_proc = struct
  type t = {
    init : CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Interproc_chan.t;
    read_chan : Interproc_chan.t;
    write_chan : Interproc_chan.t option;
  }
  [@@deriving sexp_of]
end

module Process = struct
  module K = struct
    type t = Chp of Flat_chp.Proc.t | Mem of Mem_proc.t [@@deriving sexp_of]
  end

  type t = { k : K.t } [@@deriving sexp_of]
end

type t = {
  processes : Process.t list;
  top_iports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
  top_oports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
}
[@@deriving sexp_of]

let of_process (process : Act.Internal_rep.Process.t) =
  let module Ir = Act.Internal_rep in
  let user_sendable_ports = Set.to_list process.iports in
  let user_readable_ports = Set.to_list process.oports in

  let next_interproc_chan_id = ref 0 in
  let interproc_chan_of_ir_chan_tbl = Ir.Chan.U.Table.create () in
  let new_interproc_chan bitwidth =
    let id = !next_interproc_chan_id in
    let id = Interproc_chan.Id.of_int id in
    incr next_interproc_chan_id;
    { Interproc_chan.id; bitwidth }
  in
  let interproc_chan_of_ir_chan ir_chan =
    Hashtbl.find_or_add interproc_chan_of_ir_chan_tbl ir_chan
      ~default:(fun () ->
        let bitwidth =
          match Ir.DType.layout ir_chan.d.dtype with
          | Bits_fixed bitwidth -> bitwidth
        in
        new_interproc_chan bitwidth)
  in

  let chp_procs, mem_maps =
    let rec helper (proc : Act.Internal_rep.Process.t) =
      match proc.inner with
      | Subprocs subprocs -> List.concat_map subprocs ~f:helper
      | Dflow_iface_on_chp chp ->
          [
            Flat_chp.of_chp chp ~dflowable:true ~new_interproc_chan
              ~interproc_chan_of_ir_chan;
          ]
      | Chp chp ->
          [
            Flat_chp.of_chp chp ~dflowable:false ~new_interproc_chan
              ~interproc_chan_of_ir_chan;
          ]
    in

    helper process |> List.unzip
  in
  let chp_procs =
    List.map chp_procs ~f:(fun proc -> { Process.k = Chp proc })
  in

  let mem_procs =
    List.concat_map mem_maps ~f:Map.to_alist
    |> Act.Internal_rep.Mem.Map.of_alist_multi |> Map.to_alist
    |> List.concat_map ~f:(fun (mem, chans) ->
           let (chans, bits), ctrl_procs =
             match chans with
             | [] -> failwith "unreachable"
             | [ (chans, bits) ] -> ((chans, bits), [])
             | chans ->
                 let chans, bits = List.unzip chans in
                 let bits = List.unzip bits in
                 let int_all_eq_val l =
                   List.all_equal l ~equal:Int.equal |> Option.value_exn
                 in
                 let idx_bits = int_all_eq_val (fst bits) in
                 let cell_bits = int_all_eq_val (snd bits) in
                 let cmd_chan = new_interproc_chan (idx_bits + 1) in
                 let write_chan = new_interproc_chan cell_bits in
                 let read_chan = new_interproc_chan cell_bits in
                 let ctrl_proc =
                   let _chans = chans in
                   failwith "TODO"
                 in
                 let chans = (cmd_chan, Some write_chan, Some read_chan) in
                 let bits = (idx_bits, cell_bits) in
                 ((chans, bits), [ ctrl_proc ])
           in
           let cmd_chan, write_chan, read_chan = chans in
           let idx_bits, cell_bits = bits in

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
               read_chan = Option.value_exn read_chan;
             }
           in
           let mem_proc = { Process.k = Mem mem_proc } in
           mem_proc :: ctrl_procs)
  in

  let top_iports =
    List.map user_sendable_ports ~f:(fun chan ->
        let interproc_chan = interproc_chan_of_ir_chan chan in
        (interproc_chan, chan))
  in
  let top_oports =
    List.map user_readable_ports ~f:(fun chan ->
        let interproc_chan = interproc_chan_of_ir_chan chan in
        (interproc_chan, chan))
  in
  let processes = List.concat [ chp_procs; mem_procs ] in
  { processes; top_iports; top_oports }

(* let check_n n ~user_sendable_ports ~user_readable_ports = (* assume n is a
   top-level statement. For now, we will just unilaterally impose dflow
   symatics. TODO require a chp node to weaken synmatics. *)

   (* First check there are no unsupported nodes *) let rec check_sup_nodes n =
   match n with | Ir.Chp.Par (_, ns) -> List.map ns ~f:check_sup_nodes |>
   Result.all_unit | Seq (_, ns) -> List.map ns ~f:check_sup_nodes |>
   Result.all_unit | Loop (_, n) -> check_sup_nodes n | SelectImm (_, branches,
   else_) -> let ns = List.map branches ~f:snd @ Option.to_list else_ in
   List.map ns ~f:check_sup_nodes |> Result.all_unit | WhileLoop (_, _, seq) |
   DoWhile (_, seq, _) -> check_sup_nodes seq | Assign (_, _, _) -> Ok () | Send
   (_, _, _) -> Ok () | Read (_, _, _) -> Ok () | Nop -> Ok () (* Factor these
   into a seperate process? *) | ReadUGMem (_, _, _, _) -> Ok () | WriteUGMem
   (_, _, _, _) -> Ok () (* These cant be supported in dflow *) |
   WaitUntilReadReady (_, _) -> Error "dflow does not support probes but has
   WaitUntilReadReady" | WaitUntilSendReady (_, _) -> Error "dflow does not
   support probes but has WaitUntilSendReady" (* Should these be supported, or
   should they just be dropped? *) | Log _ | Log1 _ | Assert _ -> Ok () in
   let%bind.Result () = check_sup_nodes n in

   let dummy_chan_of_mem_table = Ir.Mem.Table.create () in let dummy_chan_of_mem
   mem = Hashtbl.find_or_add dummy_chan_of_mem_table mem ~default:(fun () ->
   Act.Chan.W.create (CInt.dtype ~bits:1) |> Ir.Chan.unwrap_w) in

   (* check that par branches dont both use the same side of the same channel *)
   let rec chans n ~r ~w = let f n = chans n ~r ~w in match n with | Ir.Chp.Par
   (_, ns) -> List.concat_map ns ~f | Seq (_, ns) -> List.concat_map ns ~f |
   Loop (_, n) -> f n | SelectImm (_, branches, else_) -> ( List.concat_map
   branches ~f:(fun (_, n) -> f n) @ match else_ with Some else_ -> f else_ |
   None -> []) | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> f seq | Assign
   (_, _, _) -> [] | Send (_, chan, _) -> if w then [ chan ] else [] | Read (_,
   chan, _) -> if r then [ chan ] else [] | Nop -> [] | ReadUGMem (_, mem, _, _)
   -> [ dummy_chan_of_mem mem ] | WriteUGMem (_, mem, _, _) -> [
   dummy_chan_of_mem mem ] | Log _ | Log1 _ | Assert _ -> [] |
   WaitUntilReadReady (_, _) | WaitUntilSendReady (_, _) -> failwith
   "unreachable: handled above" in let r_chans n = chans n ~r:true ~w:false |>
   Ir.Chan.U.Set.of_list in let w_chans n = chans n ~r:false ~w:true |>
   Ir.Chan.U.Set.of_list in

   let subsets_2 l = List.mapi l ~f:(fun i x -> (i, x)) |> List.concat_map
   ~f:(fun (i, x) -> List.drop l (i + 1) |> List.map ~f:(fun y -> (x, y))) in
   let find_conflicting_pair ns ~f = let ns = List.map ns ~f:(fun n -> (n, f n))
   in subsets_2 ns |> List.filter_map ~f:(fun ((n1, l1), (n2, l2)) -> match
   Set.inter l1 l2 |> Set.to_list with | [] -> None | x :: _ -> Some (n1, n2,
   x)) |> List.hd in let rec check_par_nodes n = match n with | Ir.Chp.Par (_,
   ns) -> ( let%bind.Result () = List.map ns ~f:check_par_nodes |>
   Result.all_unit in let%bind.Result () = match find_conflicting_pair ns
   ~f:r_chans with | None -> Ok () | Some (_, _, _) -> Error "Two branches of
   par block read the same channel. Dataflow \ converter does not suppor this"
   in match find_conflicting_pair ns ~f:w_chans with | None -> Ok () | Some (_,
   _, _) -> Error "Two branches of par block write the same channel. Dataflow \
   converter does not suppor this.") | Seq (_, ns) -> List.map ns
   ~f:check_par_nodes |> Result.all_unit | Loop (_, n) -> check_par_nodes n |
   SelectImm (_, branches, else_) -> let ns = List.map branches ~f:snd @
   Option.to_list else_ in List.map ns ~f:check_par_nodes |> Result.all_unit |
   WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> check_par_nodes seq | Assign
   (_, _, _) -> Ok () | Send (_, _, _) -> Ok () | Read (_, _, _) -> Ok () | Nop
   -> Ok () (* Factor these into a seperate process? *) | ReadUGMem (_, _, _, _)
   -> Ok () | WriteUGMem (_, _, _, _) -> Ok () (* Should these be supported, or
   should they just be dropped? *) | Log _ | Log1 _ | Assert _ -> Ok () (* These
   cant be supported in dflow *) | WaitUntilReadReady (_, _) |
   WaitUntilSendReady (_, _) -> failwith "unreachable: handled above" in (* TODO
   also check variables not used same side of par node *) let%bind.Result () =
   check_par_nodes n in

   (* Then check that each io channel is not also read/written in the program.
   *) let%bind.Result () = match Set.inter user_readable_ports (r_chans n) |>
   Set.to_list with | [] -> Ok () | _ :: _ -> Error "Channel read in prgram but
   listed as user readable.. Dataflow \ converter does not suppor this." in
   match Set.inter user_sendable_ports (w_chans n) |> Set.to_list with | [] ->
   Ok () | _ :: _ -> Error "Channel written in prgram but listed as user
   sendable. Dataflow \ converter does not suppor this." *)
