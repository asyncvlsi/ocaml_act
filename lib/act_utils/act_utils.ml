open! Core
open! Random

module Any : sig
  type t [@@deriving sexp]
end = struct
  type t [@@deriving sexp]
end

module Vec = struct
  type 'a t = { mutable len : int; mutable arr : 'a array; default : 'a }
  [@@deriving sexp]

  let to_array t = Array.sub t.arr ~pos:0 ~len:t.len

  let sexp_of_t f t =
    sexp_of_t f { len = t.len; arr = to_array t; default = t.default }

  let create ~cap ~default =
    { len = 0; arr = Array.init cap ~f:(fun _ -> default); default }

  let at t n = t.arr.(n)
  let set t n v = t.arr.(n) <- v

  let remove t n =
    assert (n >= 0);
    assert (n < t.len);
    t.arr.(n) <- t.arr.(t.len - 1);
    t.len <- t.len - 1

  let push t v =
    (if Int.equal t.len (Array.length t.arr) then
     let new_len = Int.max (Array.length t.arr * 2) 16 in
     let arr =
       Array.init new_len ~f:(fun i ->
           if i < t.len then t.arr.(i) else t.default)
     in
     t.arr <- arr);
    t.arr.(t.len) <- v;
    t.len <- t.len + 1

  let extend t l = List.iter l ~f:(fun v -> push t v)
  let is_empty t = Int.equal t.len 0
  let length t = t.len
end

module Assem : sig
  module Id : sig
    type t

    include Identifiable with type t := t

    val of_int : int -> t
    val to_int : t -> int
  end

  module Expr : sig
    type t = Var of Id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
    [@@deriving sexp]

    val const : 'a -> t
    val map : t -> f:('a -> 'b) -> t
  end

  module Instr_idx : sig
    type t

    val dummy_val : t
    val next : t -> t
  end

  module Chan_buff : sig
    type t = {
      mutable read_ready : bool;
      mutable read_n_instr : Instr_idx.t;
      mutable read_dst_var_id : Id.t;
      mutable send_ready : bool;
      mutable send_n_instr : Instr_idx.t;
      mutable send_expr : Expr.t;
    }
  end

  module Par_join : sig
    type t

    val create : max_ct:int -> t
  end

  module N : sig
    type t =
      | End
      | Unreachable
      | ConsumePC
      | Assign of Id.t * Expr.t
      | Log of Expr.t
      | Assert of Expr.t
      | Par of Instr_idx.t list
      | ParJoin of Par_join.t
      | Jump of Instr_idx.t
      | JumpIfFalse of Expr.t * Instr_idx.t
      | Read of Id.t * Chan_buff.t
      | Send of Expr.t * Chan_buff.t
    [@@deriving sexp]

    val var_ids : t -> Id.t list
  end

  module Builder : sig
    type t

    val create : unit -> t
    val push : t -> N.t -> Instr_idx.t
    val edit : t -> Instr_idx.t -> N.t -> unit
    val next_idx : t -> Instr_idx.t
    val to_array : t -> N.t array
  end

  module Sim : sig
    type t

    val create : ?seed:int -> N.t array -> var_ct:int -> t
    val step : t -> bool
    val wait : ?max_steps:int -> t -> bool

    module Advanced : sig
      val add_pc : t -> Instr_idx.t -> unit
      val set_var : t -> var_id:Id.t -> value:Any.t -> unit
    end
  end
end = struct
  module Id = Int

  module Expr = struct
    type t = Var of Id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
    [@@deriving sexp]

    let sexp_of_t _ = String.sexp_of_t "<expr>"
    let const c = Const (Obj.magic c)
    let map e ~f = Map (e, Obj.magic f)

    let rec var_ids t =
      match t with Var id -> [ id ] | Const _ -> [] | Map (e, _) -> var_ids e
  end

  module Instr_idx = struct
    include Int

    let dummy_val = -1
    let next i = i + 1
  end

  module Chan_buff = struct
    type t = {
      mutable read_ready : bool;
      mutable read_n_instr : Instr_idx.t;
      mutable read_dst_var_id : Id.t;
      mutable send_ready : bool;
      mutable send_n_instr : Instr_idx.t;
      mutable send_expr : Expr.t;
    }
    [@@deriving sexp]
  end

  module Par_join = struct
    type t = { max_ct : int; mutable curr_ct : int } [@@deriving sexp]

    let create ~max_ct = { max_ct; curr_ct = 0 }
  end

  module N = struct
    type t =
      | End
      | Unreachable
      | ConsumePC
      | Assign of Id.t * Expr.t
      | Log of Expr.t
      | Assert of Expr.t
      | Par of Instr_idx.t list
      | ParJoin of Par_join.t
      | Jump of Instr_idx.t
      | JumpIfFalse of Expr.t * Instr_idx.t
      | Read of Id.t * Chan_buff.t
      | Send of Expr.t * Chan_buff.t
    [@@deriving sexp]

    let var_ids t =
      match t with
      | End | Unreachable | ConsumePC | Par _ | ParJoin _ | Jump _ -> []
      | Assign (id, expr) -> id :: Expr.var_ids expr
      | Log expr | Assert expr | JumpIfFalse (expr, _) -> Expr.var_ids expr
      | Read (var_id, _) -> [ var_id ]
      | Send (expr, _) -> Expr.var_ids expr
  end

  module Builder = struct
    type t = N.t Vec.t

    let create () = Vec.create ~cap:10 ~default:N.End

    let push t instr =
      Vec.push t instr;
      Vec.length t - 1

    let edit t idx assem = Vec.set t idx assem
    let next_idx t = Vec.length t
    let to_array t = Vec.to_array t
  end

  module Var_table : sig
    type t [@@deriving sexp]

    val create : ct:int -> t
    val assign : t -> var_id:Id.t -> expr:Expr.t -> unit
    val set : t -> var_id:Id.t -> value:Any.t -> unit
    val eval : t -> Expr.t -> Any.t
  end = struct
    type t = (Any.t option array[@sexp.opaque]) [@@deriving sexp]

    let rec eval t expr =
      match expr with
      | Expr.Var id -> (
          match t.(id) with
          | Some v -> v
          | None -> failwith "attempted to read uninitialized variable")
      | Const c -> c
      | Map (e, f) -> f (eval t e)

    let create ~ct = Array.init ct ~f:(fun _ -> None)
    let set t ~var_id ~value = t.(var_id) <- Some value
    let assign t ~var_id ~expr = set t ~var_id ~value:(eval t expr)
  end

  module Sim = struct
    type t = {
      assem : N.t array;
      mutable pcs : Instr_idx.t Vec.t;
      mutable var_table : Var_table.t;
      rng : (Random.State.t[@sexp.opaque]);
    }
    [@@deriving sexp]

    let create ?(seed = 12345678) assem ~var_ct =
      let var_table = Var_table.create ~ct:var_ct in
      let pcs = Vec.create ~cap:1000 ~default:(-1) in
      let rng = Random.State.make [| seed |] in
      Vec.push pcs 0;
      { assem; pcs; var_table; rng }

    let step t ~pc_idx =
      let vec_incr t n = t.Vec.arr.(n) <- t.arr.(n) + 1 in
      let step_chan (chan : Chan_buff.t) =
        if chan.read_ready && chan.send_ready then (
          chan.read_ready <- false;
          chan.send_ready <- false;
          Var_table.assign t.var_table ~var_id:chan.read_dst_var_id
            ~expr:chan.send_expr;
          Vec.extend t.pcs [ chan.read_n_instr; chan.send_n_instr ])
      in
      let pc = Vec.at t.pcs pc_idx in
      match t.assem.(pc) with
      | End ->
          Vec.remove t.pcs pc_idx;
          assert (Vec.is_empty t.pcs)
      | ConsumePC -> Vec.remove t.pcs pc_idx
      | Unreachable -> failwith "reached an 'unreachable' instruction"
      | Assign (var_id, expr) ->
          Var_table.assign t.var_table ~var_id ~expr;
          vec_incr t.pcs pc_idx
      | Assert expr ->
          assert (Var_table.eval t.var_table expr |> Obj.magic);
          vec_incr t.pcs pc_idx
      | Log expr ->
          printf "%s" (Var_table.eval t.var_table expr |> Obj.magic);
          vec_incr t.pcs pc_idx
      | Par instrs ->
          Vec.remove t.pcs pc_idx;
          Vec.extend t.pcs instrs
      | ParJoin d ->
          d.curr_ct <- d.curr_ct + 1;
          if Int.equal d.max_ct d.curr_ct then (
            d.curr_ct <- 0;
            vec_incr t.pcs pc_idx)
          else Vec.remove t.pcs pc_idx
      | Jump inst -> Vec.set t.pcs pc_idx inst
      | JumpIfFalse (expr, inst) -> (
          match Var_table.eval t.var_table expr |> Obj.magic with
          | true -> vec_incr t.pcs pc_idx
          | false -> Vec.set t.pcs pc_idx inst)
      | Read (dst_id, chan) ->
          assert (not chan.read_ready);
          chan.read_ready <- true;
          chan.read_n_instr <- pc + 1;
          chan.read_dst_var_id <- dst_id;
          Vec.remove t.pcs pc_idx;
          step_chan chan
      | Send (expr, chan) ->
          assert (not chan.send_ready);
          chan.send_ready <- true;
          chan.send_n_instr <- pc + 1;
          chan.send_expr <- expr;
          Vec.remove t.pcs pc_idx;
          step_chan chan

    let step t =
      let stuck = Vec.is_empty t.pcs in
      (if not stuck then
       let pc_idx = Random.State.int_incl t.rng 0 (Vec.length t.pcs - 1) in
       step t ~pc_idx);
      (* print_s [%sexp ((stuck, t.pcs): bool * Vec.t)]; *)
      not stuck

    let wait ?(max_steps = 1000) t =
      let ct = ref 0 in
      while !ct < max_steps && step t do
        incr ct
      done;
      Int.(equal !ct max_steps)

    module Advanced = struct
      let add_pc t pc = Vec.push t.pcs pc
      let set_var t ~var_id ~value = Var_table.set t.var_table ~var_id ~value
    end
  end

  let%expect_test "test0" =
    let var0 = Id.of_int 0 in
    let var1 = Id.of_int 1 in
    let assem =
      [|
        N.Assign (var0, Expr.const 3);
        Assign (var1, Expr.const "7");
        Assign (var0, Expr.map (Var var0) ~f:Int.to_string);
        Par [ 4; 6 ];
        Log (Var var0);
        Jump 7;
        Log (Var var1);
        ParJoin (Par_join.create ~max_ct:2);
        End;
      |]
    in
    let sim = Sim.create assem ~var_ct:3 in
    let timed_out = Sim.wait sim in
    printf "timed out = %s" (if timed_out then "YES" else "NO");
    [%expect {|
    73timed out = NO |}]
end

module IR = struct
  module Id : sig
    type t

    include Identifiable with type t := t

    val of_int : int -> t
    val to_assem_id : t -> Assem.Id.t
  end = struct
    include Int

    let to_assem_id t = Assem.Id.of_int t
  end

  module Chan_id : sig
    type t

    include Identifiable with type t := t

    val of_int : int -> t
  end = struct
    include Id
  end

  module Expr = struct
    type t = Var of Id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
    [@@deriving sexp]

    let sexp_of_t _ = String.sexp_of_t "<expr>"
    let const c = Const (Obj.magic c)
    let map e ~f = Map (e, Obj.magic f)

    let rec var_ids t =
      match t with Var id -> [ id ] | Const _ -> [] | Map (e, _) -> var_ids e

    let rec to_assem_expr = function
      | Var var -> Assem.Expr.Var (Id.to_assem_id var)
      | Const c -> Const c
      | Map (v, f) -> Map (to_assem_expr v, f)
  end

  module N = struct
    type t =
      | Assign of Id.t * Expr.t
      | Log of Expr.t
      | Assert of Expr.t
      | Seq of t list
      | Par of t list
      | IfElse of Expr.t * (* true *) t * (* false *) t
      | Read of (* chan *) Chan_id.t * (* var *) Id.t
      | Send of (* chan *) Chan_id.t * (* var *) Expr.t
      | Loop of t
      | WhileLoop of Expr.t * t
  end

  module Sim = struct
    type t = {
      sim : Assem.Sim.t;
      user_readable_tbl :
        (Assem.Id.t * Assem.Instr_idx.t * Assem.Chan_buff.t) Chan_id.Table.t;
      user_sendable_tbl :
        (Assem.Id.t * Assem.Instr_idx.t * Assem.Chan_buff.t) Chan_id.Table.t;
    }

    let assem_of_ir t =
      (* TODO use a vector *)
      let module AB = Assem.Builder in
      let instrs = AB.create () in
      (* let push assem =
           instrs.(!instr_idx) <- assem;
           incr instr_idx;
           !instr_idx - 1
         in
         let edit idx assem = instrs.(idx) <- assem in
         let next_idx () = !instr_idx in *)
      let chan_tbl = Chan_id.Table.create () in
      let get_chan chan_id =
        Hashtbl.find_or_add chan_tbl chan_id ~default:(fun () ->
            {
              (* Use ready_r and ready_s to check that both endpoints are set *)
              Assem.Chan_buff.read_ready = true;
              send_ready = true;
              read_n_instr = Assem.Instr_idx.dummy_val;
              read_dst_var_id = Assem.Id.of_int 0;
              send_n_instr = Assem.Instr_idx.dummy_val;
              send_expr = Const (Obj.magic 0);
            })
      in
      let rec convert stmt =
        let convert' stmt = convert stmt |> fun _ -> () in
        match stmt with
        | N.Assign (id, expr) ->
            AB.push instrs
              (Assem.N.Assign (Id.to_assem_id id, Expr.to_assem_expr expr))
        | Log expr -> AB.push instrs (Log (Expr.to_assem_expr expr))
        | Assert expr -> AB.push instrs (Assert (Expr.to_assem_expr expr))
        | Seq stmts -> List.map stmts ~f:convert |> List.last_exn
        | Par stmts ->
            let split = AB.push instrs (Par []) in
            let ends =
              List.map stmts ~f:(fun stmt ->
                  convert' stmt;
                  AB.push instrs (Jump Assem.Instr_idx.dummy_val))
            in
            let starts =
              List.take (split :: ends) (List.length stmts)
              |> List.map ~f:Assem.Instr_idx.next
            in
            let merge =
              AB.push instrs
                (ParJoin (Assem.Par_join.create ~max_ct:(List.length stmts)))
            in
            AB.edit instrs split (Par starts);
            List.iter ends ~f:(fun end_ -> AB.edit instrs end_ (Jump merge));
            merge
        | IfElse (expr, t_branch, f_branch) ->
            let split =
              AB.push instrs
                (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.dummy_val))
            in
            convert' t_branch;
            let t_end = AB.push instrs (Jump Assem.Instr_idx.dummy_val) in
            let f_end = convert f_branch in
            AB.edit instrs split
              (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.next t_end));
            AB.edit instrs t_end (Jump (Assem.Instr_idx.next f_end));
            f_end
        | Read (chan, var) ->
            let chan = get_chan chan in
            let read = AB.push instrs (Read (Id.to_assem_id var, chan)) in
            chan.read_ready <- false;
            read
        | Send (chan, expr) ->
            let chan = get_chan chan in
            let send = AB.push instrs (Send (Expr.to_assem_expr expr, chan)) in
            chan.send_ready <- false;
            send
        | Loop seq ->
            let fst = AB.next_idx instrs in
            convert' seq;
            let jmp = AB.push instrs (Jump fst) in
            jmp
        | WhileLoop (expr, seq) ->
            let split =
              AB.push instrs
                (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.dummy_val))
            in
            convert' seq;
            let jmp = AB.push instrs (Jump split) in
            AB.edit instrs split
              (JumpIfFalse (Expr.to_assem_expr expr, Assem.Instr_idx.next jmp));
            jmp
      in
      let (_ : Assem.Instr_idx.t) = convert t in
      let (_ : Assem.Instr_idx.t) = AB.push instrs End in
      (* let instr = Array.sub instrs ~pos:0 ~len:(len + 1) in *)
      (instrs, chan_tbl)

    let create ir ~user_sendable_ports ~user_readable_ports =
      (* user_sendable_ports = inputs to the program, and user_readable_ports are outputs from the program *)
      assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);
      let assem, chan_tbl = assem_of_ir ir in
      let var_ct =
        (Assem.Builder.to_array assem
        |> Array.to_list
        |> List.concat_map ~f:Assem.N.var_ids
        |> List.max_elt ~compare:Assem.Id.compare
        |> Option.map ~f:Assem.Id.to_int
        |> Option.value ~default:0)
        + 1
      in
      let var_ct = ref var_ct in
      let user_readable_tbl = Chan_id.Table.create () in
      Set.iter user_readable_ports ~f:(fun port ->
          let chan = Hashtbl.find_exn chan_tbl port in
          assert (chan.read_ready && not chan.send_ready);
          let var_id = Assem.Id.of_int !var_ct in
          incr var_ct;
          let inst_idx = Assem.Builder.push assem (Read (var_id, chan)) in
          let (_ : Assem.Instr_idx.t) = Assem.Builder.push assem ConsumePC in
          Hashtbl.set user_readable_tbl ~key:port ~data:(var_id, inst_idx, chan);
          chan.read_ready <- false);
      let user_sendable_tbl = Chan_id.Table.create () in
      Set.iter user_sendable_ports ~f:(fun port ->
          let chan = Hashtbl.find_exn chan_tbl port in
          assert ((not chan.read_ready) && chan.send_ready);
          let var_id = Assem.Id.of_int !var_ct in
          incr var_ct;
          let inst_idx = Assem.Builder.push assem (Send (Var var_id, chan)) in
          let (_ : Assem.Instr_idx.t) = Assem.Builder.push assem ConsumePC in
          Hashtbl.set user_sendable_tbl ~key:port ~data:(var_id, inst_idx, chan);
          chan.send_ready <- false);
      Hashtbl.iteri chan_tbl ~f:(fun ~key:_ ~data:chan ->
          assert ((not chan.read_ready) && not chan.send_ready));
      let sim =
        Assem.Sim.create (Assem.Builder.to_array assem) ~var_ct:!var_ct
      in
      { sim; user_sendable_tbl; user_readable_tbl }

    let wait ?(max_steps = 1000) t = Assem.Sim.wait ~max_steps t.sim

    let send t chan_id value =
      let var_id, inst_idx, _chan =
        Hashtbl.find_exn t.user_sendable_tbl chan_id
      in
      Assem.Sim.Advanced.set_var t.sim ~var_id ~value;
      Assem.Sim.Advanced.add_pc t.sim inst_idx;
      (* TODO handle checking that send actually happens *)
      ()

    let read t chan_id _value =
      let _var_id, inst_idx, _chan =
        Hashtbl.find_exn t.user_sendable_tbl chan_id
      in
      (* Var_table.set t.sim.var_table var_id val; *)
      Assem.Sim.Advanced.add_pc t.sim inst_idx;
      (* TODO handle checking that read actually happens, and that it matches the expected value *)
      ()
  end

  type t = N.t
end

let%expect_test "test1" =
  let open IR in
  let ir =
    let var0 = IR.Id.of_int 0 in
    let var1 = IR.Id.of_int 1 in
    let chan2 = IR.Chan_id.of_int 2 in
    IR.N.Seq
      [
        Assign (var0, IR.Expr.const 12345);
        Par
          [
            Loop (Seq [ Send (chan2, Var var0) ]);
            Seq
              [
                Read (chan2, var1);
                Log (Var var1 |> IR.Expr.map ~f:Int.to_string);
              ];
          ];
      ]
  in
  let sim =
    Sim.create ir ~user_sendable_ports:Chan_id.Set.empty
      ~user_readable_ports:Chan_id.Set.empty
  in
  let timed_out = IR.Sim.wait sim in
  printf "timed out = %s" (if timed_out then "YES" else "NO");
  [%expect {|
    12345timed out = NO |}]

let%expect_test "test2" =
  let open IR in
  let ir =
    let var0 = IR.Id.of_int 0 in
    let var1 = IR.Id.of_int 1 in
    IR.N.Seq
      [
        Assign (var0, Expr.const 123456);
        Assign (var1, Expr.const 1);
        WhileLoop
          ( Expr.map (Var var0) ~f:(fun v -> v > 1),
            Seq
              [
                Assign (var1, Expr.map (Var var1) ~f:(fun v -> v + 1));
                IfElse
                  ( Expr.map (Var var0) ~f:(fun v -> v % 2 < 1),
                    Assign (var0, Expr.map (Var var0) ~f:(fun v -> v / 2)),
                    Assign (var0, Expr.map (Var var0) ~f:(fun v -> (v * 3) + 1))
                  );
              ] );
        Log (Var var1 |> Expr.map ~f:(fun v -> sprintf "%d\n" v));
      ]
  in
  let sim =
    IR.Sim.create ir ~user_sendable_ports:Chan_id.Set.empty
      ~user_readable_ports:Chan_id.Set.empty
  in
  let timed_out = IR.Sim.wait sim in
  printf "timed out = %s" (if timed_out then "YES" else "NO");
  [%expect {|
    62
    timed out = NO |}]

let%expect_test "test3" =
  let open IR in
  let var = IR.Id.of_int 0 in
  let chan = IR.Chan_id.of_int 1 in
  let ir =
    IR.N.Seq
      [
        Log (Expr.const "start\n");
        Read (chan, var);
        Log (Expr.const "recv 1\n");
        Read (chan, var);
        Log (Expr.const "recv 2\n");
        Read (chan, var);
        Log (Expr.const "done\n");
      ]
  in
  let sim =
    IR.Sim.create ir
      ~user_sendable_ports:(Chan_id.Set.of_list [ chan ])
      ~user_readable_ports:Chan_id.Set.empty
  in
  let (_ : bool) = IR.Sim.wait sim in
  [%expect {| start |}];
  IR.Sim.send sim chan (Obj.magic 100);
  let (_ : bool) = IR.Sim.wait sim in
  [%expect {| recv 1 |}];
  IR.Sim.send sim chan (Obj.magic 100);
  let (_ : bool) = IR.Sim.wait sim in
  [%expect {| recv 2 |}];
  let (_ : bool) = IR.Sim.wait sim in
  [%expect {| |}];
  IR.Sim.send sim chan (Obj.magic 200);
  let (_ : bool) = IR.Sim.wait sim in
  [%expect {| done |}]
(* printf "timed out = %s" (if timed_out then "YES" else "NO"); *)

(*
   module Spliter : sig
     module I = A1.CInt
     module O = A2.CInt

     val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A1.CInt
     module O = A2.CInt

     let chp ctx ~in_ ~(out : O.W.t) =
       let b = Var.new_bool ctx in
       let v = Var.new_of_dtype ctx (I.dtype in_) in
       Chp.loop
         [
           Chp.read in_ v;
           Chp.select_bool b ~f:(fun b ->
               let o = if not b then out.a0 else out.a1 in
               [ Chp.send_var o v ]);
           Chp.flip b;
         ]

     let inst ctx ~label ~in_ =
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp

     let%expect_test "chp block" =
       let dtype = DType.new_int ~max_bits:(Bits.of_int 3) in
       let ctx = Context.create_root () in
       let in_ = I.new_of_dtype ctx dtype in
       let out = O.W.new_of_dtype ctx dtype in
       let chp = chp ctx ~in_ ~out in
       Chp.hum_print_chp chp;
       [%expect
         {|
         (Loop
          ((Read
            (((ctx ((parent ()) (id 1))) (id 2) (dtype ((max_bits 3))))
             ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3))))))
           (BoolSelect ((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1))))
            ((Send
              (((ctx ((parent ()) (id 1))) (id 3) (dtype ((max_bits 3))))
               (Var ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3))))))))
            ((Send
              (((ctx ((parent ()) (id 1))) (id 4) (dtype ((max_bits 3))))
               (Var ((ctx ((parent ()) (id 1))) (id 6) (dtype ((max_bits 3)))))))))
           (Assign
            (((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1))))
             (BoolNot
              (Var ((ctx ((parent ()) (id 1))) (id 5) (dtype ((max_bits 1)))))))))) |}]

     let sim ~max_bits =
       let dtype = DType.new_int ~max_bits in
       let ctx = Context.create_root () in
       let in_ = I.new_of_dtype ctx dtype in
       let in_w = I.get_writable_pair in_ in
       let out, node = inst ~label:"root" ctx ~in_ in
       (in_w, out, Sim.sim node)

     let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:
           [ Sim.D.new_w in_ (CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 3; 5; 7; 9 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 2; 4; 6; 8; 10 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 70, characters 10-61
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:[ Sim.D.new_w in_ (CInt.l_of_l [ 1; 0; 1; 0; 1 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 1; 1 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 0; 0 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 91, characters 10-51
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test1" =
       (* The circuit passes the value on as expected. We dont specify the order
          between the read and the write. The order between subsequent actions on
          the same channel is preserved, but there is no guarentee between
          different channels *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read sim out.a1 (CInt.of_int 321);
       Sim.read sim out.a0 (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 321);
       Sim.wait sim;
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 111, characters 4-41
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test2" =
       (* checks that we can send strictly before we can read *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 321);
       Sim.read_and_wait sim out.a1 (CInt.of_int 321);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 131, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test3" =
       (* checks that we cannot read before we send *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 150, characters 4-50
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test4" =
       (* checks that we cannot send twice before reading *)
       let in_, _out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 167, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]
   end

   module Merger : sig
     module I = A2.CInt
     module O = A1.CInt

     val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A2.CInt
     module O = A1.CInt

     let chp ctx ~in_ ~out =
       let b = Var.new_bool ctx in
       let v = Var.new_of_dtype ctx (I.dtype in_) in
       Chp.loop
         [
           Chp.select_bool b ~f:(function
             | true -> [ Chp.read in_.a0 v ]
             | false -> [ Chp.read in_.a1 v ]);
           Chp.flip b;
           Chp.send out (Expr.of_var v);
         ]

     let inst ctx ~label ~in_ =
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp
   end

   module Buffer : sig
     module I = A1.CInt
     module O = A1.CInt

     val inst : Context.t -> depth:int -> label:string -> in_:I.t -> O.t * Node.t
   end = struct
     module I = A1.CInt
     module O = A1.CInt

     let rec inst ctx ~depth ~label ~in_ =
       let nodes ctx ~in_ ~out =
         if depth < 0 then failwith "depth <= 0"
         else if Int.equal depth 0 then [ Node.tie ctx in_ out ]
         else
           let split_out, split = Spliter.inst ctx ~label:"split" ~in_ in
           let buff0_out, buff0 =
             inst ctx ~depth:(depth - 1) ~label:"left" ~in_:split_out.a0
           in
           let buff1_out, buff1 =
             inst ctx ~depth:(depth - 1) ~label:"right" ~in_:split_out.a1
           in
           let out_, merge =
             Merger.inst ctx ~label:"merge"
               ~in_:{ Merger.I.a0 = buff0_out; a1 = buff1_out }
           in
           [ split; buff0; buff1; merge; Node.tie ctx out_ out ]
       in
       let out = O.new_of_dtype ctx (I.dtype in_) in
       Node.create_cluster (module I) (module O) ctx ~label ~in_ ~out ~f:nodes
   end
*)

(*
   let sim ~depth ~mb =
     let in_ = I.new_ ~mb () in
     let out, node = inst ~depth ~in_:in_.r in
     ((in_.w, out), Sim.sim node)

   let%expect_test "test0" =
     let (in_, out), sim = sim ~depth:2 ~mb:(Bits.of_int 4) in
     let l =
       CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 7; 6; 6; 5; 4; 3; 2; 12; 1 ]
     in
     Sim.d_test sim ~inputs:[ Sim.D.new_w in_ l ] ~expected:[ Sim.D.new_r out l ];
     [%expect {||}]

   let%expect_test "test1" =
     let (in_, out), sim = sim ~depth:3 ~mb:(Bits.of_int 1) in
     let l = CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 0; 1; 2; 3; 4; 5; 6 ] in
     List.iter l ~f:(fun v -> Sim.send_and_wait sim in_ v);
     List.iter l ~f:(fun v -> Sim.read_and_wait sim out v);
     [%expect {||}] *)

(*
   module DBuffer = struct
     let rec inst ~depth ~in_ =
       if depth <= 0 then failwith "depth <= 0";
       let subinst ~in_ =
         match depth with 1 -> (in_, []) | _ -> inst ~depth:(depth - 1) ~in_
       in
       let split_out, split = Spliter.inst ~in_ in
       let buff1_out, buff1 = subinst ~in_:split_out.out_1 in
       let buff2_out, buff2 = subinst ~in_:split_out.out_2 in
       let out, merge = Merger.inst ~in_1:buff1_out ~in_2:buff2_out in
       (out, [ split; merge ] @ buff1 @ buff2)

     let inst ~depth ~in_ =
       let out, nodes = inst ~depth ~in_ in
       (out, Node_inst.of_subnodes nodes)

     let sim ~depth ~mb =
       let in_ = CInt.Chan.P.new_ ~mb () in
       let out, node = inst ~depth ~in_:in_.r in
       ((in_.w, out), Sim.sim node)

     let%expect_test "test0" =
       let (in_, out), sim = sim ~depth:2 ~mb:(Bits.of_int 4) in
       let l =
         CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 7; 6; 6; 5; 4; 3; 2; 12; 1 ]
       in
       Sim.d_test sim ~inputs:[ Sim.D.new_w in_ l ] ~expected:[ Sim.D.new_r out l ];
       [%expect {||}]

     let%expect_test "test1" =
       let (in_, out), sim = sim ~depth:3 ~mb:(Bits.of_int 1) in
       let l = CInt.l_of_l [ 1; 2; 3; 4; 5; 6; 7; 0; 1; 2; 3; 4; 5; 6 ] in
       List.iter l ~f:(fun v -> Sim.send_and_wait sim in_ v);
       List.iter l ~f:(fun v -> Sim.read_and_wait sim out v);
       [%expect {||}]
   end

   module ALU = struct
     module Instr = struct
       type t = Add | Sub | Mul | IDiv | UMinus | UNot

       let bits = Bits.of_int 4
       let all = [ Add; UMinus ]
       let arg_ct = function Add | Sub | Mul | IDiv -> 2 | UMinus | UNot -> 1

       let op_expr instr a0 a1 =
         match instr with
         | Add -> Expr.(add a0 a1)
         | Sub -> Expr.(sub a0 a1)
         | Mul -> Expr.(mul a0 a1)
         | IDiv -> Expr.(div a0 a1)
         | UMinus -> Expr.(uminus a1)
         | UNot -> Expr.(unot a0)

       module Expr = struct
         let of_var _ = failwith "unimplemented"
         let eq _ _ = failwith "unimplemented"
       end
     end

     let bits = Bits.of_int 16

     let inst ~instr ~arg_0 ~arg_1 =
       let open Chp in
       let res_0 = CInt.Chan.P.new_ ~mb:bits () in
       Block.create_3_1 ~in_:(instr, arg_0, arg_1) ~out:res_0
         ~f:(fun c_instr c_arg_0 c_arg_1 c_res_0 ->
           let instr = CInt.Var.new_ ~mb:Instr.bits () in
           let a0 = CInt.Var.new_ ~mb:bits () in
           let a1 = CInt.Var.new_ ~mb:bits () in
           let load_args instr =
             match Instr.arg_ct instr with
             | 1 -> Chp.read c_arg_0 a0
             | 2 -> Chp.par [ Chp.read c_arg_0 a0; Chp.read c_arg_1 a1 ]
             | _ -> failwith "unsupported"
           in
           let op_expr instr a0 a1 =
             Instr.op_expr instr (Expr.of_var a0) (Expr.of_var a1)
           in
           Chp.loop
             [
               Chp.read c_instr instr;
               Instr.Chp.match_ instr ~f:(fun instr ->
                   [ load_args instr; Chp.send c_res_0 (op_expr instr) ]);
             ])

     let inst () =
       let open Chp in
       let instr = CInt.Chan.P.new_ ~mb:Instr.bits () in
       let arg_0 = CInt.Chan.P.new_ ~mb:bits () in
       let arg_1 = CInt.Chan.P.new_ ~mb:bits () in
       let res_0, node = inst ~instr ~arg_0 ~arg_1 in
       ((instr, arg_0, arg_1, res_0), node)
   end
*)

(*
module Test_log : sig
  module I = A0
  module O = A0

  val inst : Context.t -> label:string -> in_:I.t -> O.t * Node.t
end = struct
  module I = A0
  module O = A0

  let chp _ ~in_:_ ~out:_ = Chp.log (Expr.const "hello world")

  let inst ctx ~label ~in_ =
    let out = O.new_ ctx in
    Node.create_chp (module I) (module O) ctx ~label ~in_ ~out ~f:chp

  let%expect_test "chp block" =
    let ctx = Context.create_root () in
    let in_ = I.new_ ctx in
    let out = O.W.new_ ctx in
    let chp = chp ctx ~in_ ~out in
    Chp.hum_print_chp chp;
    [%expect {|
      (Log (Const "hello world")) |}]

  let sim ~max_bits:_ =
    let ctx = Context.create_root () in
    let in_ = I.new_ ctx in
    let in_w = I.get_writable_pair in_ in
    let out, node = inst ~label:"root" ctx ~in_ in
    (in_w, out, Sim.sim node)

  let%expect_test "test0" =
    let _, _, sim = sim ~max_bits:(Bits.of_int 4) in
    print_s [%sexp (sim : Sim.t)];
    [%expect
      {|
      ((program (Par ((Log (Const "hello world")))))
       (pc ((parent ()) (state (Index 0)) (children ())))
       (vars ((ids ()) (tbl ()))) (chans ((ids ()) (tbl ())))) |}];
    Sim.wait sim;
    [%expect
      {|
      ((parent ()) (state (Index 0)) (children ()))
      1
      (((parent ()) (state (Index 0)) (children ()))) |}]
  (* Sim.d_test sim ~inputs:[] ~expected:[]; *)

  (* let%expect_test "test0" =
       let in_, out, sim = sim ~max_bits:(Bits.of_int 4) in
       Sim.d_test sim
         ~inputs:[ Sim.D.new_w in_ (CInt.l_of_l [ 1; 0; 1; 0; 1 ]) ]
         ~expected:
           [
             Sim.D.new_r out.a0 (CInt.l_of_l [ 1; 1; 1 ]);
             Sim.D.new_r out.a1 (CInt.l_of_l [ 0; 0 ]);
           ];
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 93, characters 10-51
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test1" =
       (* The circuit passes the value on as expected. We dont specify the order
          between the read and the write. The order between subsequent actions on
          the same channel is preserved, but there is no guarentee between
          different channels *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read sim out.a1 (CInt.of_int 321);
       Sim.read sim out.a0 (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 123);
       Sim.send sim in_ (CInt.of_int 321);
       Sim.wait sim;
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 114, characters 4-41
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test2" =
       (* checks that we can send strictly before we can read *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 321);
       Sim.read_and_wait sim out.a1 (CInt.of_int 321);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 135, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test3" =
       (* checks that we cannot read before we send *)
       let in_, out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.read_and_wait sim out.a0 (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 155, characters 4-50
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

     let%expect_test "test4" =
       (* checks that we cannot send twice before reading *)
       let in_, _out, sim = sim ~max_bits:(Bits.of_int 12) in
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       Sim.send_and_wait sim in_ (CInt.of_int 123);
       [%expect.unreachable]
       [@@expect.uncaught_exn
         {|
       (* CR expect_test_collector: This test expectation appears to contain a backtrace.
          This is strongly discouraged as backtraces are fragile.
          Please change this test to not include a backtrace. *)

       (Failure unimplemented)
       Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
       Called from Act_utils.Spliter.(fun) in file "lib/act_utils/act_utils.ml", line 173, characters 4-47
       Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}] *)
end
*)
