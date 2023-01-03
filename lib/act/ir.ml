open! Core

module Code_pos = struct
  type t = Caml.Printexc.location = {
    filename : string;
    line_number : int;
    start_char : int;
    end_char : int;
  }
  [@@deriving sexp]

  let dummy_loc =
    { filename = ""; line_number = -1; start_char = -1; end_char = -1 }
end

let get_call_site_at_idx ~idx =
  Caml.Printexc.get_callstack 3
  |> Caml.Printexc.raw_backtrace_entries
  |> (fun arr -> arr.(idx))
  |> Caml.Printexc.backtrace_slots_of_raw_entry |> Option.value_exn
  |> (fun arr -> arr.(0))
  |> Caml.Printexc.Slot.location |> Option.value_exn

let get_call_site () = get_call_site_at_idx ~idx:2

module DType : sig
  type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }

  val int_ : int t
end = struct
  type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }

  let create ~(equal : 'a -> 'a -> bool) ~(sexp_of_t : 'a -> Sexp.t) : 'a t =
    { equal = Obj.magic equal; sexp_of_t = Obj.magic sexp_of_t }

  let int_ = create ~equal:Int.equal ~sexp_of_t:Int.sexp_of_t
end

module type CH = sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Chan = struct
  module U = struct
    module T = struct
      type t = {
        id : int;
        dtype :
          (Any.t DType.t
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
        creation_code_pos :
          (Code_pos.t
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let next_int = ref 0

    let create dtype creation_code_pos =
      let id = !next_int in
      incr next_int;
      { id; dtype = Obj.magic dtype; creation_code_pos }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype (get_call_site ()) }
end

module Var = struct
  module U = struct
    module T = struct
      type t = {
        id : int;
        dtype :
          (Any.t DType.t
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
        creation_code_pos :
          (Code_pos.t
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let next_int = ref 0

    let create dtype creation_code_pos =
      let id = !next_int in
      incr next_int;
      { id; dtype = Obj.magic dtype; creation_code_pos }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype (get_call_site ()) }
end

module Expr = struct
  type 'a t =
    | Var of 'a Var.t
    | Const of Any.t
    | Map of Any.t t * (Any.t -> 'a)

  let sexp_of_t _ _ = String.sexp_of_t "<expr>"
  let const c = Const (Obj.magic c)
  let map (e : 'a t) ~(f : 'a -> 'b) = Map (Obj.magic e, Obj.magic f)
  let untype t : Any.t t = Obj.magic t

  module U = struct
    type nonrec t = Any.t t
  end
end

module N = struct
  type t =
    | Assign of Code_pos.t * Var.U.t * Any.t Expr.t
    | Log of Code_pos.t * string Expr.t
    | Assert of Code_pos.t * bool Expr.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | IfElse of Code_pos.t * bool Expr.t * (* true *) t * (* false *) t
    | Read of Code_pos.t * Chan.U.t * Var.U.t
    | Send of Code_pos.t * Chan.U.t * Expr.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * bool Expr.t * t

  let assign var_id expr =
    Assign (get_call_site (), var_id.Var.u, Expr.untype expr)

  let log expr = Log (get_call_site (), expr)
  let assert_ expr = Assert (get_call_site (), expr)
  let seq l = Seq (get_call_site (), l)
  let par l = Par (get_call_site (), l)
  let if_else expr t_br f_br = IfElse (get_call_site (), expr, t_br, f_br)
  let read chan_id var_id = Read (get_call_site (), chan_id.Chan.u, var_id.Var.u)

  let send chan_id (expr : 'a Expr.t) =
    Send (get_call_site (), chan_id.Chan.u, Expr.untype expr)

  let loop t = Loop (get_call_site (), t)
  let while_loop expr t = WhileLoop (get_call_site (), expr, t)
end
(*
   module Code_pos = struct
     type t = Lexing.position = {
       pos_fname : string;
       pos_lnum : int;
       pos_bol : int;
       pos_cnum : int;
     }
     [@@deriving sexp]

     let dummy_loc = [%here]
   end *)

module Assem_builder = struct
  type t = {
    assem : Assem.Assem_vec.t;
    mutable next_assem_var_id : int;
    var_id_to_assem_id : Assem.Var_id.t Var.U.Table.t;
  }

  let create () =
    {
      assem = Assem.Assem_vec.create ();
      next_assem_var_id = 0;
      var_id_to_assem_id = Var.U.Table.create ();
    }

  let push t instr = Assem.Assem_vec.push t.assem instr
  let edit t idx assem = Assem.Assem_vec.edit t.assem idx assem
  let next_idx t = Assem.Assem_vec.next_idx t.assem
  let assem_array t = Assem.Assem_vec.to_array t.assem

  let new_assem_var_id t =
    let id = t.next_assem_var_id in
    t.next_assem_var_id <- t.next_assem_var_id + 1;
    Assem.Var_id.of_int id

  let to_assem_id t var_id =
    Hashtbl.find_or_add t.var_id_to_assem_id var_id ~default:(fun () ->
        new_assem_var_id t)

  let rec to_assem_expr t expr =
    match expr with
    | Expr.Var var_id -> Assem.Expr.Var (to_assem_id t var_id.u)
    | Const c -> Const c
    | Map (v, f) -> Map (to_assem_expr t v, f)

  let assem_var_ct t = t.next_assem_var_id

  let assem_id_to_var_id t =
    Hashtbl.to_alist t.var_id_to_assem_id
    |> List.map ~f:(fun (var_id, assem_id) -> (assem_id, var_id))
    |> Assem.Var_id.Table.of_alist_exn
end

module Sim = struct
  module Wait_outcome = struct
    type t =
      | Already_errored
      | Time_out
      | Stuck
      | Assert_failure of Code_pos.t
      | Uninit_id of Var.U.t * Code_pos.t
      | Simul_chan_senders of Code_pos.t * Code_pos.t
      | Simul_chan_readers of Code_pos.t * Code_pos.t
      | Simul_read_write of Var.U.t * Code_pos.t * Code_pos.t
      | Simul_write_write of Var.U.t * Code_pos.t * Code_pos.t
      | Select_no_guards_true of Code_pos.t
      | Select_multiple_guards_true of int list * Code_pos.t
      | User_read_failed_to_complete of Chan.U.t
      | User_write_failed_to_complete of Chan.U.t
      | User_read_incorrect_value of Chan.U.t
      | Read_dequeuer_wrong_value of Chan.U.t * Sexp.t * Sexp.t
      | Read_dequeuer_not_done of Chan.U.t
      | Send_enqueuer_not_done of Chan.U.t
    [@@deriving sexp]
  end

  type t = {
    sim : Assem.Sim.t;
    user_readable_tbl : (* read_instr : *) Assem.Instr_idx.t Chan.U.Table.t;
    user_sendable_tbl : (* send_instr : *) Assem.Instr_idx.t Chan.U.Table.t;
    to_send_at_next_wait : (Chan.U.t * Any.t) Queue.t;
    to_read_at_next_wait : (Chan.U.t * Any.t) Queue.t;
    assem_to_loc_tbl : Code_pos.t Assem.Instr_idx.Table.t;
    assem_id_to_var_id : Var.U.t Assem.Var_id.Table.t;
    mutable is_done : bool;
  }

  let assem_of_ir t =
    let module Instr_idx = Assem.Instr_idx in
    let module AB = Assem_builder in
    let ab = AB.create () in
    let assem_to_loc_tbl = Assem.Instr_idx.Table.create () in
    let push loc instr =
      let idx = AB.push ab instr in
      Hashtbl.set assem_to_loc_tbl ~key:idx ~data:loc;
      idx
    in
    let edit loc idx instr =
      AB.edit ab idx instr;
      Hashtbl.set assem_to_loc_tbl ~key:idx ~data:loc
    in
    let chan_tbl = Chan.U.Table.create () in
    let get_chan (chan_id : Chan.U.t) =
      Hashtbl.find_or_add chan_tbl chan_id ~default:(fun () ->
          Assem.Chan_buff.create ())
    in
    let convert_id (id : Var.U.t) = AB.to_assem_id ab id in
    let convert_expr expr = AB.to_assem_expr ab (Expr.untype expr) in
    (* Set up the initial jump. This is required by the Assem.Sim module. *)
    let () =
      let init_jump = push Code_pos.dummy_loc (Jump Instr_idx.dummy_val) in
      let start = AB.next_idx ab in
      edit Code_pos.dummy_loc init_jump (Jump start)
    in
    let rec convert stmt =
      let convert' stmt = ignore (convert stmt : Instr_idx.t) in
      match stmt with
      | N.Assign (loc, id, expr) ->
          push loc (Assign (convert_id id, convert_expr expr))
      | Log (loc, expr) -> push loc (Log (convert_expr expr))
      | Assert (loc, expr) -> push loc (Assert (convert_expr expr))
      | Seq (loc, stmts) -> (
          match stmts with
          | [] -> push loc Nop
          | stmts -> List.map stmts ~f:convert |> List.last_exn)
      | Par (loc, stmts) ->
          let split = push loc (Par []) in
          let ends =
            List.map stmts ~f:(fun stmt ->
                convert' stmt;
                push loc (Jump Instr_idx.dummy_val))
          in
          let starts =
            List.take (split :: ends) (List.length stmts)
            |> List.map ~f:Instr_idx.next
          in
          let merge =
            push loc
              (ParJoin (Assem.Par_join.create ~max_ct:(List.length stmts)))
          in
          edit loc split (Par starts);
          List.iter ends ~f:(fun end_ -> edit loc end_ (Jump merge));
          merge
      | IfElse (loc, expr, t_branch, f_branch) ->
          let split =
            push loc (JumpIfFalse (convert_expr expr, Instr_idx.dummy_val))
          in
          convert' t_branch;
          let t_end = push loc (Jump Instr_idx.dummy_val) in
          let f_end = convert f_branch in
          edit loc split (JumpIfFalse (convert_expr expr, Instr_idx.next t_end));
          edit loc t_end (Jump (Instr_idx.next f_end));
          f_end
      | Read (loc, chan, var) ->
          let chan = get_chan chan in
          push loc (Read (convert_id var, chan))
      | Send (loc, chan, expr) ->
          let chan = get_chan chan in
          push loc (Send (convert_expr expr, chan))
      | Loop (loc, seq) ->
          let fst = AB.next_idx ab in
          convert' seq;
          push loc (Jump fst)
      | WhileLoop (loc, expr, seq) ->
          let split =
            push loc (JumpIfFalse (convert_expr expr, Instr_idx.dummy_val))
          in
          convert' seq;
          let jmp = push loc (Jump split) in
          edit loc split (JumpIfFalse (convert_expr expr, Instr_idx.next jmp));
          jmp
    in
    let (_ : Instr_idx.t) = convert t in
    let (_ : Instr_idx.t) = push Code_pos.dummy_loc End in
    (ab, chan_tbl, assem_to_loc_tbl)

  let create ir ~user_sendable_ports ~user_readable_ports =
    (* user_sendable_ports = inputs to the program, and user_readable_ports are outputs from the program *)
    assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
    let ab, chan_tbl, assem_to_loc_tbl = assem_of_ir ir in
    let user_readable_tbl = Chan.U.Table.create () in
    Set.iter user_readable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem_builder.new_assem_var_id ab in
        let read_instr = Assem_builder.push ab (Read (var_id, chan)) in
        let dequeuer =
          Assem.Read_dequeuer.create ~var_id ~equals:port.dtype.equal
        in
        let _ = Assem_builder.push ab (Read_dequeuer dequeuer) in
        Hashtbl.set user_readable_tbl ~key:port ~data:read_instr);
    let user_sendable_tbl = Chan.U.Table.create () in
    Set.iter user_sendable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem_builder.new_assem_var_id ab in
        let send_instr = Assem_builder.push ab (Send (Var var_id, chan)) in
        let enqueuer = Assem.Send_enqueuer.create ~var_id in
        let _ = Assem_builder.push ab (Send_enqueuer enqueuer) in
        Hashtbl.set user_sendable_tbl ~key:port ~data:send_instr);
    let var_ct = Assem_builder.assem_var_ct ab in
    let sim = Assem.Sim.create (Assem_builder.assem_array ab) ~var_ct in
    {
      sim;
      user_sendable_tbl;
      user_readable_tbl;
      to_send_at_next_wait = Queue.create ();
      to_read_at_next_wait = Queue.create ();
      is_done = false;
      assem_id_to_var_id = Assem_builder.assem_id_to_var_id ab;
      assem_to_loc_tbl;
    }

  let to_ordered_alist_map queue =
    Queue.to_list queue |> Chan.U.Map.of_alist_multi
    |> Map.map ~f:(fun l -> List.rev l |> Queue.of_list)

  let wait ?(max_steps = 1000) t =
    let to_send = to_ordered_alist_map t.to_send_at_next_wait in
    let to_read = to_ordered_alist_map t.to_read_at_next_wait in
    Queue.clear t.to_send_at_next_wait;
    Queue.clear t.to_read_at_next_wait;
    Map.iteri to_send ~f:(fun ~key:chan_id ~data:values ->
        assert (Queue.length values > 0);
        let send_instr = Hashtbl.find_exn t.user_sendable_tbl chan_id in
        Assem.Sim.Advanced.set_user_sends t.sim ~values ~send_instr);
    Map.iteri to_read ~f:(fun ~key:chan_id ~data:values ->
        assert (Queue.length values > 0);
        let read_instr = Hashtbl.find_exn t.user_readable_tbl chan_id in
        Assem.Sim.Advanced.set_user_reads t.sim ~values ~read_instr);
    let decode_instr instr_idx =
      Hashtbl.find_exn t.assem_to_loc_tbl instr_idx
    in
    let decode_var_id assem_id =
      Hashtbl.find_exn t.assem_id_to_var_id assem_id
    in
    if t.is_done then Wait_outcome.Already_errored
    else
      let status =
        match Assem.Sim.wait ~max_steps t.sim with
        | Assem.Sim.Wait_outcome.Already_errored -> assert false
        | Stuck _ -> Wait_outcome.Stuck
        | Time_out -> Time_out
        | Assert_failure inst_idx -> Assert_failure (decode_instr inst_idx)
        | Uninit_id (var_id, inst_idx) ->
            Uninit_id (decode_var_id var_id, decode_instr inst_idx)
        | Simul_chan_senders (instr_1, instr_2) ->
            Simul_chan_senders (decode_instr instr_1, decode_instr instr_2)
        | Simul_chan_readers (instr_1, instr_2) ->
            Simul_chan_readers (decode_instr instr_1, decode_instr instr_2)
        | Simul_read_write (var_id, instr_1, instr_2) ->
            Simul_read_write
              (decode_var_id var_id, decode_instr instr_1, decode_instr instr_2)
        | Simul_write_write (var_id, instr_1, instr_2) ->
            Simul_write_write
              (decode_var_id var_id, decode_instr instr_1, decode_instr instr_2)
        | Select_no_guards_true inst_idx ->
            Select_no_guards_true (decode_instr inst_idx)
        | Select_multiple_guards_true (true_idxs, inst_idx) ->
            Select_multiple_guards_true (true_idxs, decode_instr inst_idx)
        | Read_dequeuer_wrong_value (dequeuer_instr, actual, expected) ->
            let chan_id, _ =
              Hashtbl.to_alist t.user_readable_tbl
              |> List.find_exn ~f:(fun (_, send_instr) ->
                     Assem.Instr_idx.equal
                       (Assem.Instr_idx.next send_instr)
                       dequeuer_instr)
            in
            (* TODO maybe get either the chan_id creation line or the line called to create this expectation *)
            Read_dequeuer_wrong_value
              ( chan_id,
                chan_id.dtype.sexp_of_t actual,
                chan_id.dtype.sexp_of_t expected )
            (* Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string *)
        | Read_dequeuer_not_done dequeuer_instr ->
            let chan_id, _ =
              Hashtbl.to_alist t.user_readable_tbl
              |> List.find_exn ~f:(fun (_, send_instr) ->
                     Assem.Instr_idx.equal
                       (Assem.Instr_idx.next send_instr)
                       dequeuer_instr)
            in
            (* TODO maybe get either the chan_id creation line or the line called to create this expectation *)
            Read_dequeuer_not_done chan_id
            (* Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string *)
        | Send_enqueuer_not_done enqueuer_instr ->
            let chan_id, _ =
              Hashtbl.to_alist t.user_sendable_tbl
              |> List.find_exn ~f:(fun (_, read_instr) ->
                     Assem.Instr_idx.equal
                       (Assem.Instr_idx.next read_instr)
                       enqueuer_instr)
            in
            (* TODO maybe get either the chan_id creation line or the line called to create this expectation *)
            Send_enqueuer_not_done chan_id
        (* Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string *)
      in
      (match status with Stuck -> () | _ -> t.is_done <- true);
      status

  let wait' ?max_steps t = ignore (wait ?max_steps t : Wait_outcome.t)

  let send t (chan_id : 'a Chan.t) (value : 'a) =
    match Hashtbl.mem t.user_sendable_tbl chan_id.u with
    | true -> Queue.enqueue t.to_send_at_next_wait (chan_id.u, Obj.magic value)
    | false ->
        failwith
          "the provided chan_id was not regestered as a user-sendable chan in \
           Sim.create"

  let read t (chan_id : 'a Chan.t) (value : 'a) =
    match Hashtbl.mem t.user_readable_tbl chan_id.u with
    | true -> Queue.enqueue t.to_read_at_next_wait (chan_id.u, Obj.magic value)
    | false ->
        failwith
          "the provided chan_id was not regestered as a user-readable chan in \
           Sim.create"
end
