open! Core

module type DTypeable = sig
  type t [@@deriving sexp_of, equal]
end

module DType = struct
  type 'a t = { equal : Any.t -> Any.t -> bool; sexp_of_t : Any.t -> Sexp.t }

  let create ~(equal : 'a -> 'a -> bool) ~(sexp_of_t : 'a -> Sexp.t) : 'a t =
    { equal = Obj.magic equal; sexp_of_t = Obj.magic sexp_of_t }

  let of_module (type a) (module M : DTypeable with type t = a) : a t =
    create ~equal:M.equal ~sexp_of_t:M.sexp_of_t

  let int_ = of_module (module Int)
  let bool_ = of_module (module Bool)
  let string_ = of_module (module String)
end

module type Comparable_and_hashable = sig
  type t [@@deriving sexp_of, compare, equal, hash]

  include Comparable with type t := t
  include Hashable with type t := t
end

module Chan_ = struct
  module U = struct
    module T = struct
      type t = {
        id : int;
        dtype :
          (Any.t DType.t
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
        creation_code_pos :
          (Code_pos.t[@hash.ignore] [@compare.ignore] [@equal.ignore]);
        (* I have not come up with a way to add which direction is passive into
             the type system. These two fields help with error reporting *)
        mutable wait_readable_code_pos :
          (Code_pos.t option[@hash.ignore] [@compare.ignore] [@equal.ignore]);
        mutable wait_sendable_code_pos :
          (Code_pos.t option[@hash.ignore] [@compare.ignore] [@equal.ignore]);
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
      {
        id;
        dtype = Obj.magic dtype;
        creation_code_pos;
        wait_readable_code_pos = None;
        wait_sendable_code_pos = None;
      }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype (Code_pos.value_or_psite loc) }
end

module Chan = struct
  module R = struct
    module U = Chan_.U

    type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

    let create = Chan_.create
  end

  module W = struct
    module U = Chan_.U

    type 'a t = 'a Chan_.t = { u : U.t } [@@deriving sexp_of]

    let create = Chan_.create
  end

  type 'a t = { r : 'a R.t; w : 'a W.t } [@@deriving sexp_of]

  let create ?loc (dtype : 'a DType.t) : 'a t =
    let c = Chan_.create ?loc dtype in
    { r = c; w = c }
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
        init :
          (Any.t option
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let next_int = ref 0

    let create dtype init creation_code_pos =
      let id = !next_int in
      incr next_int;
      {
        id;
        dtype = Obj.magic dtype;
        creation_code_pos;
        init = Option.map init ~f:Obj.magic;
      }
  end

  type 'a t = { u : U.t } [@@deriving sexp_of]

  let create ?loc ?init (dtype : 'a DType.t) : 'a t =
    { u = U.create dtype init (Code_pos.value_or_psite loc) }
end

module UnguardedMem = struct
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
        init :
          (Any.t array
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
        kind :
          ([ `Mem | `Rom ]
          [@hash.ignore] [@compare.ignore] [@equal.ignore] [@sexp.opaque]);
      }
      [@@deriving hash, compare, equal, sexp]
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let next_int = ref 0

    let create dtype init creation_code_pos kind =
      let id = !next_int in
      incr next_int;
      { id; dtype = Obj.magic dtype; creation_code_pos; init; kind }
  end

  type 'a t = { u : U.t } [@@deriving sexp]

  let create ?loc dtype arr =
    { u = U.create dtype (Obj.magic arr) (Code_pos.value_or_psite loc) `Mem }
end

module UnguardedRom = struct
  module U = UnguardedMem.U

  type 'a t = { u : U.t } [@@deriving sexp]

  let create ?loc dtype arr =
    { u = U.create dtype (Obj.magic arr) (Code_pos.value_or_psite loc) `Rom }
end

module Expr = struct
  type 'a t =
    | Var : 'a Var.t -> 'a t
    | Const : 'a -> 'a t
    | Map : Any.t t * (Any.t -> 'a) -> 'a t
    | Add : int t * int t -> int t
    | Sub : int t * int t -> int t
    | Mul : int t * int t -> int t
    | Div : int t * int t -> int t
    | Mod : int t * int t -> int t
    | LShift : int t * int t -> int t
    | LogicalRShift : int t * int t -> int t
    | ArithRShift : int t * int t -> int t
    | BitAnd : int t * int t -> int t
    | BitOr : int t * int t -> int t
    | BitXor : int t * int t -> int t
    | Eq : int t * int t -> bool t
    | Ne : int t * int t -> bool t
    | Not : bool t -> bool t
  [@@deriving sexp_of]

  (* internal *)
  let untype t : Any.t t = Obj.magic t

  module U = struct
    type nonrec t = Any.t t
  end

  (* main operations *)
  let var v = Var v
  let const c = Const c
  let map (e : 'a t) ~(f : 'a -> 'b) = Map (untype e, Obj.magic f)

  (* ops *)
  let add a b = Add (a, b)
  let sub a b = Sub (a, b)
  let mul a b = Mul (a, b)
  let div a b = Div (a, b)
  let mod_ a b = Mod (a, b)
  let lshift a ~amt = LShift (a, amt)

  let rshift a ~amt ~arith =
    if arith then ArithRShift (a, amt) else LogicalRShift (a, amt)

  let bit_and a b = BitAnd (a, b)
  let bit_or a b = BitOr (a, b)
  let bit_xor a b = BitXor (a, b)
  let eq a b = Eq (a, b)
  let ne a b = Ne (a, b)
  let not_ a = Not a
end

module N = struct
  type t =
    | Assign of Code_pos.t * Var.U.t * Any.t Expr.t
    | Log of Code_pos.t * string Expr.t
    | Assert of Code_pos.t * bool Expr.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | Read of Code_pos.t * Chan_.U.t * Var.U.t
    | Send of Code_pos.t * Chan_.U.t * Expr.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * bool Expr.t * t
    | SelectImm of Code_pos.t * (bool Expr.t * t) list * t option
    | ReadUGMem of Code_pos.t * UnguardedMem.U.t * int Expr.t * Var.U.t
    | WriteUGMem of Code_pos.t * UnguardedMem.U.t * int Expr.t * Expr.U.t
    | ReadUGRom of Code_pos.t * UnguardedRom.U.t * int Expr.t * Var.U.t

  let assign ?loc var_id expr =
    Assign (Code_pos.value_or_psite loc, var_id.Var.u, Expr.untype expr)

  let toggle ?loc var_id = assign ?loc var_id Expr.(var var_id |> not_)
  let incr ?loc var_id = assign ?loc var_id Expr.(var var_id |> add (const 1))

  let read ?loc chan_id var_id =
    Read (Code_pos.value_or_psite loc, chan_id.Chan_.u, var_id.Var.u)

  let send ?loc chan_id (expr : 'a Expr.t) =
    Send (Code_pos.value_or_psite loc, chan_id.Chan_.u, Expr.untype expr)

  let send' ?loc chan_id var_id = send ?loc chan_id Expr.(var var_id)

  (* interacting with memories *)
  let read_ug_mem ?loc (mem : 'a UnguardedMem.t) ~idx ~(dst : 'a Var.t) =
    ReadUGMem (Code_pos.value_or_psite loc, mem.u, idx, dst.u)

  let write_ug_mem ?loc (mem : 'a UnguardedMem.t) ~idx ~(value : 'a Expr.t) =
    WriteUGMem (Code_pos.value_or_psite loc, mem.u, idx, Expr.untype value)

  let write_ug_mem' ?loc (mem : 'a UnguardedMem.t) ~idx ~(value : 'a Var.t) =
    write_ug_mem ?loc mem ~idx ~value:Expr.(var value)

  let read_ug_rom ?loc (rom : 'a UnguardedRom.t) ~idx ~(dst : 'a Var.t) =
    ReadUGRom (Code_pos.value_or_psite loc, rom.u, idx, dst.u)

  let log ?loc expr = Log (Code_pos.value_or_psite loc, expr)

  let assert_ ?loc expr =
    Assert (Option.value loc ~default:(Code_pos.psite ()), expr)

  let seq ?loc l = Seq (Code_pos.value_or_psite loc, l)
  let par ?loc l = Par (Code_pos.value_or_psite loc, l)

  let if_else ?loc expr t_br f_br =
    SelectImm
      ( Code_pos.value_or_psite loc,
        [ (expr, seq ?loc t_br) ],
        Some (seq ?loc f_br) )

  let loop ?loc t = Loop (Code_pos.value_or_psite loc, seq ?loc t)

  let while_loop ?loc expr t =
    WhileLoop (Code_pos.value_or_psite loc, expr, seq ?loc t)

  let select_imm ?loc branches ~else_ =
    SelectImm (Code_pos.value_or_psite loc, branches, else_)
end

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

  let to_assem_expr t expr =
    let rec convert : 'a. 'a Expr.t -> Assem.Expr.t =
      let imap2 a b f = Assem.Expr.map2 (convert a) (convert b) ~f in
      fun (type a) (x : a Expr.t) ->
        match x with
        | Expr.Var var_id -> Assem.Expr.Var (to_assem_id t var_id.u)
        | Const c -> Const (Any.of_magic c)
        | Map (v, f) -> Assem.Expr.map (convert v) ~f
        | Add (a, b) -> imap2 a b Int.( + )
        | Sub (a, b) -> imap2 a b Int.( - )
        | Mul (a, b) -> imap2 a b Int.( * )
        | Div (a, b) -> imap2 a b Int.( / )
        | Mod (a, b) -> imap2 a b Int.( % )
        | LShift (a, b) -> imap2 a b Int.shift_left
        | LogicalRShift (a, b) -> imap2 a b Int.shift_right
        | ArithRShift (a, b) -> imap2 a b Int.shift_right_logical
        | BitAnd (a, b) -> imap2 a b Int.bit_and
        | BitOr (a, b) -> imap2 a b Int.bit_or
        | BitXor (a, b) -> imap2 a b Int.bit_xor
        | Eq (a, b) -> imap2 a b Int.equal
        | Ne (a, b) -> imap2 a b (fun a b -> not (Int.equal a b))
        | Not a -> Assem.Expr.map (convert a) ~f:not
    in
    convert expr

  let assem_var_ct t = t.next_assem_var_id

  let var_of_assem_id t =
    Hashtbl.to_alist t.var_id_to_assem_id
    |> List.map ~f:(fun (var_id, assem_id) -> (assem_id, var_id))
    |> Assem.Var_id.Table.of_alist_exn
end

module Sim = struct
  module Wait_error = struct
    type t =
      | Already_errored
      | Time_out
      | Assert_failure of Code_pos.t
      | Uninit_id of Var.U.t * Code_pos.t
      | Simul_chan_senders of Code_pos.t * Code_pos.t
      | Simul_chan_readers of Code_pos.t * Code_pos.t
      | Simul_read_write of Var.U.t * Code_pos.t * Code_pos.t
      | Simul_write_write of Var.U.t * Code_pos.t * Code_pos.t
      | Select_no_guards_true of Code_pos.t
      | Select_multiple_guards_true of int list * Code_pos.t
      | User_read_failed_to_complete of Chan_.U.t
      | User_write_failed_to_complete of Chan_.U.t
      | User_read_incorrect_value of Chan_.U.t
      | Read_dequeuer_wrong_value of Chan_.U.t * Sexp.t * Sexp.t * Code_pos.t
      | Read_dequeuer_not_done of Chan_.U.t * Code_pos.t
      | Send_enqueuer_not_done of Chan_.U.t * Code_pos.t
    [@@deriving sexp]
  end

  type t = {
    sim : Assem.Sim.t;
    read_instr_of_chan : (* read_instr : *) Assem.Instr_idx.t Chan_.U.Table.t;
    send_instr_of_chan : (* send_instr : *) Assem.Instr_idx.t Chan_.U.Table.t;
    code_pos_of_instr : Code_pos.t Assem.Instr_idx.Table.t;
    var_of_assem_id : Var.U.t Assem.Var_id.Table.t;
    mutable to_send_at_next_wait : (Chan_.U.t * (Any.t * Code_pos.t)) Queue.t;
    mutable to_read_at_next_wait : (Chan_.U.t * (Any.t * Code_pos.t)) Queue.t;
    mutable is_done : bool;
  }
  [@@deriving sexp_of]

  let chan_of_instr mp instr =
    Hashtbl.to_alist mp
    |> List.find_exn ~f:(fun (_, i) -> Assem.Instr_idx.(equal (next i) instr))
    |> fst

  let chan_of_read_instr t instr = chan_of_instr t.read_instr_of_chan instr
  let chan_of_send_instr t instr = chan_of_instr t.send_instr_of_chan instr

  let assem_of_ir t =
    let module Instr_idx = Assem.Instr_idx in
    let module AB = Assem_builder in
    let ab = AB.create () in
    let code_pos_of_instr = Assem.Instr_idx.Table.create () in
    let push loc instr =
      let idx = AB.push ab instr in
      Hashtbl.set code_pos_of_instr ~key:idx ~data:loc;
      idx
    in
    let edit loc idx instr =
      AB.edit ab idx instr;
      Hashtbl.set code_pos_of_instr ~key:idx ~data:loc
    in
    let chan_tbl = Chan_.U.Table.create () in
    let get_chan (chan_id : Chan_.U.t) =
      Hashtbl.find_or_add chan_tbl chan_id ~default:(fun () ->
          Assem.Chan_buff.create ())
    in
    let mem_tbl = UnguardedMem.U.Table.create () in
    let get_mem (mem : UnguardedMem.U.t) =
      Hashtbl.find_or_add mem_tbl mem ~default:(fun () ->
          Assem.Mem_buff.create ~init:mem.init
            ~idx_helper_reg:(AB.new_assem_var_id ab))
    in
    let get_rom = get_mem in

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
      let push_branches loc stmts =
        let split = push loc Nop in
        let ends =
          List.map stmts ~f:(fun stmt ->
              convert' stmt;
              push loc (Jump Instr_idx.dummy_val))
        in
        let starts =
          List.take (split :: ends) (List.length stmts)
          |> List.map ~f:Instr_idx.next
        in
        let merge = push loc Nop in
        List.iter ends ~f:(fun end_ -> edit loc end_ (Jump merge));
        (split, starts, merge)
      in
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
          let split, starts, merge = push_branches loc stmts in
          edit loc split (Par starts);
          edit loc merge
            (ParJoin (Assem.Par_join.create ~max_ct:(List.length stmts)));
          merge
      | SelectImm (loc, branches, else_) -> (
          match else_ with
          | Some else_ ->
              let guards, stmts = List.unzip branches in
              let split, starts, merge = push_branches loc (else_ :: stmts) in
              let guards = List.map guards ~f:convert_expr in
              let guards = List.zip_exn guards (List.tl_exn starts) in
              edit loc split (SelectImmElse (guards, List.hd_exn starts));
              merge
          | None ->
              let guards, stmts = List.unzip branches in
              let split, starts, merge = push_branches loc stmts in
              let guards = List.map guards ~f:convert_expr in
              let guards = List.zip_exn guards starts in
              edit loc split (SelectImm guards);
              merge)
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
      | ReadUGMem (loc, mem, idx, dst) ->
          let mem = get_mem mem in
          push loc (ReadMem (convert_expr idx, convert_id dst, mem))
      | WriteUGMem (loc, mem, idx, value) ->
          let mem = get_mem mem in
          push loc (WriteMem (convert_expr idx, convert_expr value, mem))
      | ReadUGRom (loc, rom, idx, dst) ->
          let mem = get_rom rom in
          push loc (ReadMem (convert_expr idx, convert_id dst, mem))
    in
    let (_ : Instr_idx.t) = convert t in
    let (_ : Instr_idx.t) = push Code_pos.dummy_loc End in
    (ab, chan_tbl, code_pos_of_instr)

  let create ir ~user_sendable_ports ~user_readable_ports =
    let user_sendable_ports = Chan_.U.Set.of_list user_sendable_ports in
    let user_readable_ports = Chan_.U.Set.of_list user_readable_ports in
    assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
    let ab, chan_tbl, code_pos_of_instr = assem_of_ir ir in
    let read_instr_of_chan = Chan_.U.Table.create () in
    Set.iter user_readable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem_builder.new_assem_var_id ab in
        let read_instr = Assem_builder.push ab (Read (var_id, chan)) in
        let dequeuer =
          Assem.Read_dequeuer.create ~var_id ~equals:port.dtype.equal
        in
        let _ = Assem_builder.push ab (Read_dequeuer dequeuer) in
        Hashtbl.set read_instr_of_chan ~key:port ~data:read_instr);
    let send_instr_of_chan = Chan_.U.Table.create () in
    Set.iter user_sendable_ports ~f:(fun port ->
        let chan = Hashtbl.find_exn chan_tbl port in
        let var_id = Assem_builder.new_assem_var_id ab in
        let send_instr = Assem_builder.push ab (Send (Var var_id, chan)) in
        let enqueuer = Assem.Send_enqueuer.create ~var_id in
        let _ = Assem_builder.push ab (Send_enqueuer enqueuer) in
        Hashtbl.set send_instr_of_chan ~key:port ~data:send_instr);
    let var_of_assem_id = Assem_builder.var_of_assem_id ab in
    let var_inits =
      Hashtbl.map var_of_assem_id ~f:(fun var_id ->
          Option.map var_id.init ~f:Any.of_magic)
    in
    let var_ct = Assem_builder.assem_var_ct ab + 1 in
    let sim =
      Assem.Sim.create (Assem_builder.assem_array ab) ~var_ct ~var_inits
    in
    {
      sim;
      send_instr_of_chan;
      read_instr_of_chan;
      to_send_at_next_wait = Queue.create ();
      to_read_at_next_wait = Queue.create ();
      is_done = false;
      var_of_assem_id = Assem_builder.var_of_assem_id ab;
      code_pos_of_instr;
    }

  let to_ordered_alist_map queue =
    Queue.to_list queue |> Chan_.U.Map.of_alist_multi

  let wait t ?(max_steps = 1000) ?(line_numbers = true) () =
    let%bind.Result () =
      if t.is_done then Error (Error.of_string "Already errored") else Ok ()
    in
    let to_send = to_ordered_alist_map t.to_send_at_next_wait in
    let to_read = to_ordered_alist_map t.to_read_at_next_wait in
    Queue.clear t.to_send_at_next_wait;
    Queue.clear t.to_read_at_next_wait;
    let user_sends =
      Map.to_alist to_send
      |> List.map ~f:(fun (chan_id, values_with_pos) ->
             let values = List.map values_with_pos ~f:fst in
             assert (List.length values > 0);
             let send_instr = Hashtbl.find_exn t.send_instr_of_chan chan_id in
             (send_instr, values))
      |> Assem.Instr_idx.Map.of_alist_exn
    in
    let user_reads =
      Map.to_alist to_read
      |> List.map ~f:(fun (chan_id, values_with_pos) ->
             let values = List.map values_with_pos ~f:fst in
             assert (List.length values > 0);
             let read_instr = Hashtbl.find_exn t.read_instr_of_chan chan_id in
             (read_instr, values))
      |> Assem.Instr_idx.Map.of_alist_exn
    in
    let decode_instr instr_idx =
      Hashtbl.find_exn t.code_pos_of_instr instr_idx
    in
    let decode_var_id assem_id = Hashtbl.find_exn t.var_of_assem_id assem_id in
    let status = Assem.Sim.wait ~max_steps t.sim ~user_sends ~user_reads in
    let _line_numbers = line_numbers in
    (* TODO *)
    let decode_error err =
      let s_of_cp (cp : Code_pos.t) =
        sprintf "in %s on line %d" cp.filename cp.line_number
      in
      let s_of_var var_id =
        (decode_var_id var_id).creation_code_pos |> s_of_cp
      in
      let s_of_chan (chan_id : Chan_.U.t) =
        chan_id.creation_code_pos |> s_of_cp
      in
      let s_of_instr instr = decode_instr instr |> s_of_cp in
      match err with
      | Assem.Sim.Wait_error.Already_errored -> assert false
      | Time_out -> "Simulation timed out. Maybe increase max_steps?"
      | Assert_failure inst_idx ->
          if line_numbers then
            sprintf "Assertion failed: %s." (s_of_instr inst_idx)
          else "Assertion failed."
      | Uninit_id (var_id, inst_idx) ->
          if line_numbers then
            sprintf "Uninitialized variable: read %s, created %s."
              (s_of_instr inst_idx) (s_of_var var_id)
          else "Uninitialized variable."
      | Simul_chan_senders (instr_1, instr_2) ->
          if line_numbers then
            sprintf
              "Simulatnious senders on channel: statement 1 %s, statement 2 %s."
              (s_of_instr instr_1) (s_of_instr instr_2)
          else "Simulatnious senders on channel."
      | Simul_chan_readers (instr_1, instr_2) ->
          if line_numbers then
            sprintf
              "Simulatnious readers on channel: statement 1 %s, statement 2 %s."
              (s_of_instr instr_1) (s_of_instr instr_2)
          else "Simulatnious readers on channel."
      | Simul_read_write (var_id, instr_1, instr_2) ->
          if line_numbers then
            sprintf
              "Simulatnious read and write of variable: statement 1 %s, \
               statement 2 %s, create %s."
              (s_of_instr instr_1) (s_of_instr instr_2) (s_of_var var_id)
          else "Simulatnious read and write of variable."
      | Simul_write_write (var_id, instr_1, instr_2) ->
          if line_numbers then
            sprintf
              "Simulatnious writes of variable: statement 1 %s, statement 2 \
               %s, create %s."
              (s_of_instr instr_1) (s_of_instr instr_2) (s_of_var var_id)
          else "Simulatnious writes of variable."
      | Select_no_guards_true inst_idx ->
          if line_numbers then
            sprintf "Select statement has no true guards: %s."
              (s_of_instr inst_idx)
          else "Select statement has no true guards."
      | Select_multiple_guards_true (true_idxs, inst_idx) ->
          if line_numbers then
            sprintf
              "Select statement has multiple true guards: %s, true branch \
               indices as %s."
              (s_of_instr inst_idx)
              ([%sexp (true_idxs : int list)] |> Sexp.to_string)
          else "Select statement has multiple true guards."
      | Read_dequeuer_wrong_value (dequeuer_instr, actual, expected, op_idx) ->
          let chan_id = chan_of_read_instr t dequeuer_instr in
          if line_numbers then
            let code_pos : Code_pos.t =
              Map.find_exn to_read chan_id
              |> (fun l -> List.nth_exn l op_idx)
              |> snd
            in
            sprintf
              "User read has wrong value: got %s, but expected %s based on \
               `send' function call %s, on chan created %s."
              (chan_id.dtype.sexp_of_t actual |> Sexp.to_string)
              (chan_id.dtype.sexp_of_t expected |> Sexp.to_string)
              (s_of_cp code_pos) (s_of_chan chan_id)
          else
            sprintf "User read has wrong value: got %s, but expected %s"
              (chan_id.dtype.sexp_of_t actual |> Sexp.to_string)
              (chan_id.dtype.sexp_of_t expected |> Sexp.to_string)
      | Read_dequeuer_not_done (dequeuer_instr, op_idx) ->
          if line_numbers then
            let chan_id = chan_of_read_instr t dequeuer_instr in
            let code_pos : Code_pos.t =
              Map.find_exn to_read chan_id
              |> (fun l -> List.nth_exn l op_idx)
              |> snd
            in
            sprintf
              "User read did not complete:  called %s, on chan created %s."
              (s_of_cp code_pos) (s_of_chan chan_id)
          else sprintf "User read did not complete."
      | Send_enqueuer_not_done (enqueuer_instr, op_idx) ->
          if line_numbers then
            let chan_id = chan_of_send_instr t enqueuer_instr in
            let code_pos : Code_pos.t =
              Map.find_exn to_send chan_id
              |> (fun l -> List.nth_exn l (op_idx - 1))
              |> snd
            in
            sprintf
              "User send did not complete:  called %s, on chan created %s."
              (s_of_cp code_pos) (s_of_chan chan_id)
          else sprintf "User send did not complete."
      | Mem_out_of_bounds (mem_instr, idx, len) ->
          if line_numbers then
            sprintf
              "Mem access out of bounds: %s, idx is %d, size of mem is %d."
              (s_of_instr mem_instr) idx len
          else
            sprintf "Mem access out of bounds: idx is %d, size of mem is %d."
              idx len
    in
    let status = Result.map_error status ~f:decode_error in
    let status = Result.map_error status ~f:Error.of_string in
    Result.iter_error status ~f:(fun _ -> t.is_done <- true);
    status

  let wait' t ?max_steps () =
    print_s [%sexp (wait t ?max_steps () : unit Or_error.t)]

  let send t ?loc (chan_id : 'a Chan_.t) (value : 'a) =
    let call_site = Code_pos.value_or_psite loc in
    match Hashtbl.mem t.send_instr_of_chan chan_id.u with
    | true ->
        Queue.enqueue t.to_send_at_next_wait
          (chan_id.u, (Any.of_magic value, call_site))
    | false ->
        failwith
          "the provided chan_id was not regestered as a user-sendable chan in \
           Sim.create"

  let read t ?loc (chan_id : 'a Chan_.t) (value : 'a) =
    let call_site = Code_pos.value_or_psite loc in
    match Hashtbl.mem t.read_instr_of_chan chan_id.u with
    | true ->
        Queue.enqueue t.to_read_at_next_wait
          (chan_id.u, (Any.of_magic value, call_site))
    | false ->
        failwith
          "the provided chan_id was not regestered as a user-readable chan in \
           Sim.create"
end

let block11 i1 o1 ~f = f i1 o1
