open! Core
module Var_id = Int

module Expr = struct
  type t = Var of Var_id.t | Const of Any.t | Map of t * (Any.t -> Any.t)
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
    mutable read_instr : Instr_idx.t;
    mutable read_dst_var_id : Var_id.t;
    mutable send_ready : bool;
    mutable send_instr : Instr_idx.t;
    mutable send_expr : Expr.t;
  }
  [@@deriving sexp]

  let create () =
    {
      read_ready = false;
      send_ready = false;
      read_instr = Instr_idx.dummy_val;
      send_instr = Instr_idx.dummy_val;
      read_dst_var_id = 0;
      send_expr = Const (Obj.magic 0);
    }
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
    | Assign of Var_id.t * Expr.t
    | Log of Expr.t
    | Assert of Expr.t
    | Par of Instr_idx.t list
    | ParJoin of Par_join.t
    | Jump of Instr_idx.t
    | JumpIfFalse of Expr.t * Instr_idx.t
    | Read of Var_id.t * Chan_buff.t
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

  module Eval_error : sig
    type t = Uninit_id of Var_id.t [@@deriving sexp]
  end

  val create : ct:int -> t
  val assign : t -> var_id:Var_id.t -> expr:Expr.t -> unit
  val set : t -> var_id:Var_id.t -> value:Any.t -> unit
  val eval : t -> Expr.t -> Any.t
end = struct
  type t = (Any.t option array[@sexp.opaque]) [@@deriving sexp]

  module Eval_error = struct
    type t = Uninit_id of Var_id.t [@@deriving sexp]
  end

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

  let assign t ~var_id ~expr =
    let value = eval t expr in
    set t ~var_id ~value
end

let for_loop_else max_ct ~f ~else_ =
  let return = ref None in
  let ct = ref 0 in
  while !ct < max_ct && Option.is_none !return do
    (match f ct with `Continue -> () | `Return d -> return := Some d);
    incr ct
  done;
  Option.value !return ~default:else_

module Sim = struct
  type t = {
    assem : N.t array;
    mutable pcs : Instr_idx.t Vec.t;
    mutable var_table : Var_table.t;
    rng : (Random.State.t[@sexp.opaque]);
  }
  [@@deriving sexp]

  module Wait_outcome = struct
    type t = Done | Time_out | Stuck | Assert_failure of Instr_idx.t
    [@@deriving sexp]
  end

  let create ?(seed = 0) assem ~var_ct =
    let var_table = Var_table.create ~ct:var_ct in
    let pcs = Vec.of_array [| 0 |] ~default:(-1) in
    let rng = Random.State.make [| seed |] in
    { assem; pcs; var_table; rng }

  let step' t ~pc_idx =
    let vec_incr vec n = Vec.set vec n (Vec.at vec n + 1) in
    let step_chan (chan : Chan_buff.t) =
      if chan.read_ready && chan.send_ready then (
        chan.read_ready <- false;
        chan.send_ready <- false;
        Var_table.assign t.var_table ~var_id:chan.read_dst_var_id
          ~expr:chan.send_expr;
        Vec.extend t.pcs [ chan.read_instr + 1; chan.send_instr + 1 ];
        `Success)
      else `Success
    in
    let pc = Vec.at t.pcs pc_idx in
    match t.assem.(pc) with
    | End ->
        Vec.remove t.pcs pc_idx;
        if not (Vec.is_empty t.pcs) then
          failwith
            "somehow reached `End` while there are still program counters. The \
             assembly is malformated.";
        `Done
    | ConsumePC ->
        Vec.remove t.pcs pc_idx;
        `Success
    | Unreachable -> failwith "reached an 'unreachable' instruction"
    | Assign (var_id, expr) ->
        Var_table.assign t.var_table ~var_id ~expr;
        vec_incr t.pcs pc_idx;
        `Success
    | Assert expr ->
        if Var_table.eval t.var_table expr |> Obj.magic then (
          vec_incr t.pcs pc_idx;
          `Success)
        else `Assert_failure pc
    | Log expr ->
        printf "%s" (Var_table.eval t.var_table expr |> Obj.magic);
        vec_incr t.pcs pc_idx;
        `Success
    | Par instrs ->
        Vec.remove t.pcs pc_idx;
        Vec.extend t.pcs instrs;
        `Success
    | ParJoin d ->
        d.curr_ct <- d.curr_ct + 1;
        if Int.equal d.max_ct d.curr_ct then (
          d.curr_ct <- 0;
          vec_incr t.pcs pc_idx)
        else Vec.remove t.pcs pc_idx;
        `Success
    | Jump inst ->
        Vec.set t.pcs pc_idx inst;
        `Success
    | JumpIfFalse (expr, inst) ->
        (match Var_table.eval t.var_table expr |> Obj.magic with
        | true -> vec_incr t.pcs pc_idx
        | false -> Vec.set t.pcs pc_idx inst);
        `Success
    | Read (dst_id, chan) ->
        assert (not chan.read_ready);
        chan.read_ready <- true;
        chan.read_instr <- pc;
        chan.read_dst_var_id <- dst_id;
        Vec.remove t.pcs pc_idx;
        step_chan chan
    | Send (expr, chan) ->
        assert (not chan.send_ready);
        chan.send_ready <- true;
        chan.send_instr <- pc;
        chan.send_expr <- expr;
        Vec.remove t.pcs pc_idx;
        step_chan chan

  let step t =
    if Vec.is_empty t.pcs then `Stuck
    else
      let pc_idx = Random.State.int_incl t.rng 0 (Vec.length t.pcs - 1) in
      step' t ~pc_idx

  let wait ?(max_steps = 1000) t =
    for_loop_else max_steps ~else_:Wait_outcome.Time_out ~f:(fun _ ->
        let res = step t in
        match res with
        | `Success -> `Continue
        | `Done -> `Return Wait_outcome.Done
        | `Stuck -> `Return Stuck
        | `Assert_failure pc -> `Return (Assert_failure pc))

  module Advanced = struct
    let add_pc t pc = Vec.push t.pcs pc
    let set_var t ~var_id ~value = Var_table.set t.var_table ~var_id ~value
  end
end

let%expect_test "test0" =
  let var0 = Var_id.of_int 0 in
  let var1 = Var_id.of_int 1 in
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
  let update_outcome = Sim.wait sim in
  print_s [%sexp (update_outcome : Sim.Wait_outcome.t)];
  [%expect {|
    37Done |}]
