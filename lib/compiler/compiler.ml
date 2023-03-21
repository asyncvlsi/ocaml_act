open! Core
module Code_pos = Act.Code_pos
module Act_ir = Act.Internal_rep

let dummy_loc = Code_pos.dummy_loc

module Compiled_program = struct
  type t = Opt_program.t [@@deriving sexp_of]

  let of_process process ~to_ =
    let program =
      Act.Internal_rep.Process.unwrap process |> Program.of_process
    in
    match to_ with
    | `Chp_and_dataflow -> Opt_program.of_prog program
    | `Prod_rules -> failwith "TODO"

  let of_chp_process chp ~user_sendable_ports ~user_readable_ports ~to_ =
    let iports =
      user_sendable_ports |> List.map ~f:Act.Internal_rep.Chan.ru_of_wu
    in
    let oports =
      user_readable_ports |> List.map ~f:Act.Internal_rep.Chan.wu_of_ru
    in
    let process =
      match to_ with
      | `Chp -> Act.Process.of_chp chp ~iports ~oports
      | `Dataflow ->
          Act.Process.of_chp ~with_dflow_interface:true chp ~iports ~oports
      | `Prod_rules -> failwith "TODO"
    in
    let to_ = `Chp_and_dataflow in
    of_process process ~to_
end

let compile process ~to_ = Compiled_program.of_process process ~to_

let compile_chp chp ~user_sendable_ports ~user_readable_ports ~to_ =
  Compiled_program.of_chp_process chp ~user_sendable_ports ~user_readable_ports
    ~to_

let export t = Opt_program.export t

let export_print t =
  let s = export t in
  printf "%s" s

let sim ?seed (t : Compiled_program.t) =
  let ir_chan_of_interproc_chan =
    Interproc_chan.Table.of_alist_exn (t.top_iports @ t.top_oports)
  in
  let of_interproc_chan interproc_chan =
    Hashtbl.find_or_add ir_chan_of_interproc_chan interproc_chan
      ~default:(fun () ->
        let dtype = Act.CInt.dtype ~bits:interproc_chan.bitwidth in
        let interproc_chan = Act.Chan.create dtype in
        Act_ir.Chan.unwrap_r interproc_chan.r)
  in

  let expr_k_of_expr e ~of_v ~bits_of_var =
    let rec f e =
      match e with
      | F_expr.Var v -> Act_ir.Expr.K.Var (of_v v)
      | Const c -> Const c
      | Add (a, b) -> Add (f a, f b)
      | Sub_no_wrap (a, b) -> Sub_no_wrap (f a, f b)
      | Mul (a, b) -> Mul (f a, f b)
      | Div (a, b) -> Div (f a, f b)
      | Mod (a, b) -> Mod (f a, f b)
      | Eq (a, b) -> Eq (f a, f b)
      | Ne (a, b) -> Ne (f a, f b)
      | Gt (a, b) -> Gt (f a, f b)
      | Ge (a, b) -> Ge (f a, f b)
      | Lt (a, b) -> Lt (f a, f b)
      | Le (a, b) -> Le (f a, f b)
      | Eq0 a -> Eq (f a, Const Act.CInt.zero)
      | BitXor (a, b) -> BitXor (f a, f b)
      | BitOr (a, b) -> BitOr (f a, f b)
      | BitAnd (a, b) -> BitAnd (f a, f b)
      | LShift (a, b) -> LShift (f a, f b)
      | RShift (a, b) -> LogicalRShift (f a, f b)
      | Clip (a, bits) -> Clip (f a, bits)
      | Concat l ->
          List.folding_map l ~init:0 ~f:(fun acc (e, bits) ->
              ( acc + bits,
                Act_ir.Expr.K.LShift
                  (Clip (f e, bits), Const (Act.CInt.of_int acc)) ))
          |> List.reduce_exn ~f:(fun a b -> BitOr (a, b))
      | Log2OneHot e ->
          let w = F_expr.bitwidth e ~bits_of_var in
          let e = f e in
          List.init w ~f:(fun idx ->
              Act_ir.Expr.K.(
                Mul
                  ( BitAnd
                      ( LogicalRShift (e, Const (Act.CInt.of_int idx)),
                        Const Act.CInt.one ),
                    Const (Act.CInt.of_int idx) )))
          |> List.reduce_exn ~f:(fun a b -> BitOr (a, b))
    in

    f e
  in

  let of_chp (chp : Flat_chp.Proc.t) =
    let ir_chan_of_chan = Flat_chp.Chan.Table.create () in
    let of_c ?interproc_chan c =
      Hashtbl.find_or_add ir_chan_of_chan c ~default:(fun () ->
          match interproc_chan with
          | Some interproc_chan -> of_interproc_chan interproc_chan
          | None ->
              let dtype = Act.CInt.dtype ~bits:c.bitwidth in
              let c = Act.Chan.create dtype in
              Act_ir.Chan.unwrap_r c.r)
    in
    List.iter chp.iports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Act_ir.Chan.U.t) = of_c chp_chan ~interproc_chan in
        ());
    List.iter chp.oports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Act_ir.Chan.U.t) = of_c chp_chan ~interproc_chan in
        ());

    let ir_var_of_var = Flat_chp.Var.Table.create () in
    let of_v v =
      Hashtbl.find_or_add ir_var_of_var v ~default:(fun () ->
          let dtype = Act.CInt.dtype ~bits:v.bitwidth in
          Act.Var.create dtype |> Act_ir.Var.untype')
    in
    let dtype_of_v v = v.Act_ir.Var.U.d.dtype in
    let dtype_of_cw c = c.Act_ir.Chan.U.d.dtype in

    let of_expr e ~dtype =
      let k = expr_k_of_expr e ~of_v ~bits_of_var:(fun v -> v.bitwidth) in
      let tag = Act_ir.DType.expr_tag dtype in
      let max_bits = match Act_ir.DType.layout dtype with Bits_fixed b -> b in
      { Act_ir.Expr.k; max_bits; tag }
    in
    let of_bool_expr e =
      of_expr e ~dtype:(Act.CBool.dtype |> Act_ir.DType.unwrap)
    in

    let rec of_stmt chp =
      match chp with
      | Flat_chp.Stmt.Nop -> Act_ir.Chp.Nop
      | Assert e -> Assert (dummy_loc, of_bool_expr e)
      | Assign (v, e) ->
          let v = of_v v in
          Assign (dummy_loc, v, of_expr e ~dtype:(dtype_of_v v))
      | Seq ns -> Seq (dummy_loc, List.map ns ~f:of_stmt)
      | Par ns -> Par (dummy_loc, List.map ns ~f:of_stmt)
      | ReadThenAssert (c, v, _) ->
          (* TODO for now just forget about the assert *)
          Read (dummy_loc, of_c c, of_v v)
      | Send (c, e) ->
          let c = of_c c in
          Send (dummy_loc, c, of_expr e ~dtype:(dtype_of_cw c))
      | DoWhile (n, e) -> DoWhile (dummy_loc, of_stmt n, of_bool_expr e)
      | SelectImm (gs, ns) ->
          let branches =
            List.zip_exn gs ns
            |> List.map ~f:(fun (g, n) -> (of_bool_expr g, of_stmt n))
          in
          SelectImm (dummy_loc, branches, None)
      | Nondeterm_select _ -> failwith "TODO"
    in
    of_stmt chp.stmt
  in

  let of_dflow (dflow : Flat_dflow.Proc.t) =
    let ir_chan_of_chan = Flat_dflow.Var.Table.create () in
    let of_c ?interproc_chan c =
      Hashtbl.find_or_add ir_chan_of_chan c ~default:(fun () ->
          match interproc_chan with
          | Some interproc_chan -> of_interproc_chan interproc_chan
          | None ->
              let dtype = Act.CInt.dtype ~bits:c.bitwidth in
              let c = Act.Chan.create dtype in
              Act_ir.Chan.unwrap_r c.r)
    in
    List.iter dflow.iports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Act_ir.Chan.U.t) = of_c chp_chan ~interproc_chan in
        ());
    List.iter dflow.oports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Act_ir.Chan.U.t) = of_c chp_chan ~interproc_chan in
        ());

    let of_c_r v = of_c v |> Act_ir.Chan.wrap_r in
    let of_c_w v = of_c v |> Act_ir.Chan.wrap_w in

    let new_var ?init bits =
      let dtype = Act.CInt.dtype ~bits in
      Act.Var.create ?init dtype
    in
    let utv v = Act_ir.Var.untype' v |> Act_ir.Var.wrap' in

    let of_stmt chp =
      match chp with
      | Flat_dflow.Stmt.MultiAssign fblock ->
          let module FBlock = Flat_dflow.FBlock in
          let var_of_in =
            FBlock.ins fblock
            |> List.map ~f:(fun i -> (i, new_var i.bitwidth))
            |> Flat_dflow.Var.Map.of_alist_exn
          in
          let exprs = FBlock.expr_list fblock in
          let do_reads =
            Map.to_alist var_of_in
            |> List.map ~f:(fun (i, ivar) -> Act.Chp.read (of_c_r i) (utv ivar))
            |> Act.Chp.par
          in
          let do_sends =
            List.map exprs ~f:(fun (dst, expr) ->
                let expr =
                  let k =
                    expr_k_of_expr expr
                      ~of_v:(fun v ->
                        Map.find_exn var_of_in v |> Act_ir.Var.untype')
                      ~bits_of_var:(fun v -> v.bitwidth)
                  in
                  let tag =
                    Act.CInt.dtype ~bits:dst.bitwidth
                    |> Act_ir.DType.unwrap |> Act_ir.DType.expr_tag
                  in
                  Act_ir.Expr.untype
                    { Act_ir.Expr.k; max_bits = dst.bitwidth; tag }
                  |> Act_ir.Expr.wrap
                in
                Act.Chp.send (of_c_w dst) expr)
            |> Act.Chp.par
          in
          Act.Chp.loop [ do_reads; do_sends ]
      | Split (g, i, os) ->
          let tmp = new_var i.bitwidth in
          let g_var = new_var g.bitwidth in
          Act.Chp.loop
            [
              Act.Chp.read (of_c_r g) (utv g_var);
              Act.Chp.read (of_c_r i) (utv tmp);
              Act.Chp.select_imm
                (List.mapi os ~f:(fun idx o ->
                     let g = Act.Expr.(eq (var g_var) (of_int idx)) in
                     let n =
                       match o with
                       | Some o -> Act.Chp.send_var (of_c_w o) (utv tmp)
                       | None -> Act.Chp.seq []
                     in
                     (g, n)))
                ~else_:None;
            ]
      | Merge (g, ins, o) ->
          let tmp = new_var o.bitwidth in
          let g_var = new_var g.bitwidth in
          Act.Chp.loop
            [
              Act.Chp.read (of_c_r g) (utv g_var);
              Act.Chp.select_imm
                (List.mapi ins ~f:(fun idx i ->
                     ( Act.Expr.(eq (var g_var) (of_int idx)),
                       Act.Chp.read (of_c_r i) (utv tmp) )))
                ~else_:None;
              Act.Chp.send_var (of_c_w o) (utv tmp);
            ]
      | Buff1 (dst, src, init) -> (
          assert (Int.equal dst.bitwidth src.bitwidth);
          match init with
          | Some init ->
              let tmp = new_var src.bitwidth ~init in
              Act.Chp.loop
                [
                  Act.Chp.send_var (of_c_w dst) (utv tmp);
                  Act.Chp.read (of_c_r src) (utv tmp);
                ]
          | None ->
              let tmp = new_var src.bitwidth in
              Act.Chp.loop
                [
                  Act.Chp.read (of_c_r src) (utv tmp);
                  Act.Chp.send_var (of_c_w dst) (utv tmp);
                ])
      | Clone (i, os) ->
          let tmp = new_var i.bitwidth in
          Act.Chp.loop
            [
              Act.Chp.read (of_c_r i) (utv tmp);
              Act.Chp.par
                (List.map os ~f:(fun o -> Act.Chp.send_var (of_c_w o) (utv tmp)));
            ]
      | Sink i ->
          let tmp = new_var i.bitwidth in
          Act.Chp.loop [ Act.Chp.read (of_c_r i) (utv tmp) ]
    in
    Act.Chp.par (List.map dflow.stmt ~f:of_stmt) |> Act_ir.Chp.unwrap
  in

  let of_mem (mem : Flat_mem.Proc.t) =
    (* init : CInt.t array; idx_bits : int; cell_bits : int; cmd_chan :
       Interproc_chan.t; read_chan : Interproc_chan.t; write_chan :
       Interproc_chan.t option; *)
    let of_c c = of_interproc_chan c |> Act_ir.Chan.wrap_'a in
    let cmd_chan = (of_c mem.cmd_chan).r in
    let read_chan = (of_c mem.read_chan).w in
    let write_chan = Option.map mem.write_chan ~f:(fun c -> (of_c c).r) in
    let cmd =
      let dtype = Act.CInt.dtype ~bits:(mem.idx_bits + 1) in
      Act.Var.create dtype
    in
    let tmp =
      let dtype = Act.CInt.dtype ~bits:mem.cell_bits in
      Act.Var.create dtype
    in
    let mem =
      Act.Mem.create_ug_mem (Act.CInt.dtype ~bits:mem.cell_bits) mem.init
    in
    Act.Chp.loop
      [
        Act.Chp.read cmd_chan cmd;
        Act.Chp.if_else
          Act.Expr.(var cmd |> bit_and one |> eq zero)
          [
            Act.Chp.read_ug_mem mem
              ~idx:Act.Expr.(var cmd |> right_shift' ~amt:1)
              ~dst:tmp;
            Act.Chp.send_var read_chan tmp;
          ]
          (match write_chan with
          | Some write_chan ->
              [
                Act.Chp.read write_chan tmp;
                Act.Chp.write_ug_mem mem
                  ~idx:Act.Expr.(var cmd |> right_shift' ~amt:1)
                  ~value:(Act.Expr.var tmp);
              ]
          | None -> [ Act.Chp.assert_ Act.Expr.false_ ]);
      ]
    |> Act_ir.Chp.unwrap
  in

  (* generate a big pile of parallel chp processes *)
  let chps =
    List.map t.processes ~f:(fun process ->
        match process with
        | Chp chp -> of_chp chp
        | Dflow dflow -> of_dflow dflow
        | Mem mem -> of_mem mem)
  in
  let chp = Act_ir.Chp.Par (dummy_loc, chps) |> Act_ir.Chp.wrap in
  let user_sendable_ports =
    List.map t.top_iports ~f:(fun (_, ir_chan) -> Act_ir.Chan.wrap_wu ir_chan)
  in
  let user_readable_ports =
    List.map t.top_oports ~f:(fun (_, ir_chan) -> Act_ir.Chan.wrap_ru ir_chan)
  in

  Sim.simulate_chp ?seed chp ~user_sendable_ports ~user_readable_ports
