open! Core

type t = Ir_process.t [@@deriving sexp_of]

let of_chp ?(with_dflow_interface = false) chp ~iports ~oports =
  let iports =
    List.map iports ~f:Chan.Internal.unwrap_ru |> Ir_chan.U.Set.of_list
  in
  let oports =
    List.map oports ~f:Chan.Internal.unwrap_wu |> Ir_chan.U.Set.of_list
  in
  match with_dflow_interface with
  | false ->
      (* TODO do checks *)
      { Ir_process.inner = Chp (Chp.Internal.unwrap chp); iports; oports }
  | true ->
      (* TODO do checks *)
      { inner = Dflow_iface_on_chp (Chp.Internal.unwrap chp); iports; oports }

let of_procs procs ~iports ~oports =
  let iports =
    List.map iports ~f:Chan.Internal.unwrap_ru |> Ir_chan.U.Set.of_list
  in
  let oports =
    List.map oports ~f:Chan.Internal.unwrap_wu |> Ir_chan.U.Set.of_list
  in
  { Ir_process.inner = Subprocs procs; iports; oports }

module Internal = struct
  let unwrap t = t
end

(* let is_dflowable n ~user_sendable_ports ~user_readable_ports = (* assume n is
   a top-level statement. For now, we will just unilaterally impose dflow
   symatics. TODO require a chp node to weaken synmatics. *)

   (* First check there are no unsupported nodes *) let rec check_sup_nodes n =
   match n with | Ir.Chp.Par (_, ns) -> List.iter ns ~f:check_sup_nodes | Seq
   (_, ns) -> List.iter ns ~f:check_sup_nodes | Loop (_, n) -> check_sup_nodes n
   | SelectImm (_, branches, else_) -> List.iter branches ~f:(fun (_, n) ->
   check_sup_nodes n); Option.iter else_ ~f:check_sup_nodes | WhileLoop (_, _,
   seq) | DoWhile (_, seq, _) -> check_sup_nodes seq | Assign (_, _, _) -> () |
   Send (_, _, _) -> () | Read (_, _, _) -> () | Nop -> () (* Factor these into
   a seperate process? *) | ReadUGMem (_, _, _, _) -> () | WriteUGMem (_, _, _,
   _) -> () (* These cant be supported in dflow *) | WaitUntilReadReady (_, _)
   -> failwith "dflow does not support probes but has WaitUntilReadReady" |
   WaitUntilSendReady (_, _) -> failwith "dflow does not support probes but has
   WaitUntilSendReady" (* Should these be supported, or should they just be
   dropped? *) | Log _ | Log1 _ | Assert _ -> () in check_sup_nodes n;

   let dummy_chan_of_mem_table = Ir.Mem.Table.create () in let dummy_chan_of_mem
   mem = Hashtbl.find_or_add dummy_chan_of_mem_table mem ~default:(fun () ->
   Chan.W.create (Cint.dtype ~bits:1) |> Ir.Chan.unwrap_w) in

   (* check that par branches dont both use the same side of the same channel *)
   let rec chans n ~r ~w = let f n = chans n ~r ~w in match n with | Ir.Chp.Par
   (_, ns) -> List.concat_map ns ~f | Seq (_, ns) -> List.concat_map ns ~f |
   Loop (_, n) -> f n | SelectImm (_, branches, else_) -> ( List.concat_map
   branches ~f:(fun (_, n) -> f n) @ match else_ with Some else_ -> f else_ |
   None -> []) | WhileLoop (_, _, seq) | DoWhile (_, seq, _) -> f seq | Assign
   (_, _, _) -> [] | Send (_, chan, _) -> if w then [ chan ] else [] | Read (_,
   chan, _) -> if r then [ chan ] else [] | Nop -> [] | ReadUGMem (_, mem, _, _)
   -> [ dummy_chan_of_mem mem ] | WriteUGMem (_, mem, _, _) -> [
   dummy_chan_of_mem mem ] | WaitUntilReadReady (_, _) -> failwith "dflow does
   not support probes but has WaitUntilReadReady" | WaitUntilSendReady (_, _) ->
   failwith "dflow does not support probes but has WaitUntilSendReady" | Log _ |
   Log1 _ | Assert _ -> [] in let r_chans n = chans n ~r:true ~w:false |>
   Ir.Chan.U.Set.of_list in let w_chans n = chans n ~r:false ~w:true |>
   Ir.Chan.U.Set.of_list in

   let subsets_2 l = List.mapi l ~f:(fun i x -> (i, x)) |> List.concat_map
   ~f:(fun (i, x) -> List.drop l (i + 1) |> List.map ~f:(fun y -> (x, y))) in
   let find_conflicting_pair ns ~f = let ns = List.map ns ~f:(fun n -> (n, f n))
   in subsets_2 ns |> List.filter_map ~f:(fun ((n1, l1), (n2, l2)) -> match
   Set.inter l1 l2 |> Set.to_list with | [] -> None | x :: _ -> Some (n1, n2,
   x)) |> List.hd in let rec check_par_nodes n = match n with | Ir.Chp.Par (_,
   ns) -> ( List.iter ns ~f:check_par_nodes; (match find_conflicting_pair ns
   ~f:r_chans with | None -> () | Some (_, _, _) -> failwith "Two branches of
   par block read the same channel. Dataflow \ converter does not suppor this");
   match find_conflicting_pair ns ~f:w_chans with | None -> () | Some (_, _, _)
   -> failwith "Two branches of par block write the same channel. Dataflow \
   converter does not suppor this.") | Seq (_, ns) -> List.iter ns
   ~f:check_par_nodes | Loop (_, n) -> check_par_nodes n | SelectImm (_,
   branches, else_) -> List.iter branches ~f:(fun (_, n) -> check_par_nodes n);
   Option.iter else_ ~f:check_par_nodes | WhileLoop (_, _, seq) | DoWhile (_,
   seq, _) -> check_par_nodes seq | Assign (_, _, _) -> () | Send (_, _, _) ->
   () | Read (_, _, _) -> () | Nop -> () (* Factor these into a seperate
   process? *) | ReadUGMem (_, _, _, _) -> () | WriteUGMem (_, _, _, _) -> () (*
   These cant be supported in dflow *) | WaitUntilReadReady (_, _) -> failwith
   "dflow does not support probes but has WaitUntilReadReady" |
   WaitUntilSendReady (_, _) -> failwith "dflow does not support probes but has
   WaitUntilSendReady" (* Should these be supported, or should they just be
   dropped? *) | Log _ | Log1 _ | Assert _ -> () in (* TODO also check variables
   not used same side of par node *) check_par_nodes n;

   (* Then check that each io channel is not also read/written in the program.
   *) (match Set.inter user_readable_ports (r_chans n) |> Set.to_list with | []
   -> () | _ :: _ -> failwith "Channel read in prgram but listed as user
   readable.. Dataflow \ converter does not suppor this."); match Set.inter
   user_sendable_ports (w_chans n) |> Set.to_list with | [] -> () | _ :: _ ->
   failwith "Channel written in prgram but listed as user sendable. Dataflow \
   converter does not suppor this." *)
