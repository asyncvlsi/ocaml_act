open! Core

let dummy_loc = Code_pos.dummy_loc

module Compiled_program = struct
  type t = Opt_program.t [@@deriving sexp_of]

  let of_process (process : Ir.Process.t) ~to_ =
    let program = process |> Program.of_process in
    match to_ with
    | `Chp_and_dataflow -> Opt_program.of_prog program
    | `Prod_rules -> failwith "TODO"
end

let compile process ~to_ = Compiled_program.of_process process ~to_
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
        Ir.Chan.create interproc_chan.bitwidth Code_pos.dummy_loc)
  in

  let expr_k_of_expr e ~of_v ~bits_of_var =
    let rec f e =
      match e with
      | F_expr.Var v -> Ir.Expr.Var (of_v v)
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
      | Eq0 a -> Eq (f a, Const Cint.zero)
      | BitXor (a, b) -> BitXor (f a, f b)
      | BitOr (a, b) -> BitOr (f a, f b)
      | BitAnd (a, b) -> BitAnd (f a, f b)
      | LShift (a, b) -> LShift (f a, f b)
      | RShift (a, b) -> LogicalRShift (f a, f b)
      | Clip (a, bits) -> Clip (f a, bits)
      | Concat l ->
          List.folding_map l ~init:0 ~f:(fun acc (e, bits) ->
              ( acc + bits,
                Ir.Expr.LShift (Clip (f e, bits), Const (Cint.of_int acc)) ))
          |> List.reduce_exn ~f:(fun a b -> BitOr (a, b))
      | Log2OneHot e ->
          let w = F_expr.bitwidth e ~bits_of_var in
          let e = f e in
          List.init w ~f:(fun idx ->
              Ir.Expr.(
                Mul
                  ( BitAnd
                      ( LogicalRShift (e, Const (Cint.of_int idx)),
                        Const Cint.one ),
                    Const (Cint.of_int idx) )))
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
          | None -> Ir.Chan.create c.bitwidth Code_pos.dummy_loc)
    in
    List.iter chp.iports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Ir.Chan.t) = of_c chp_chan ~interproc_chan in
        ());
    List.iter chp.oports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Ir.Chan.t) = of_c chp_chan ~interproc_chan in
        ());

    let ir_var_of_var = Flat_chp.Var.Table.create () in
    let of_v v =
      Hashtbl.find_or_add ir_var_of_var v ~default:(fun () ->
          Ir.Var.create v.bitwidth Code_pos.dummy_loc None)
    in

    (* let dtype_of_cw c = c.Ir.Chan.d.dtype in *)
    let of_expr e = expr_k_of_expr e ~of_v ~bits_of_var:(fun v -> v.bitwidth) in
    let of_bool_expr e = of_expr e in

    let rec of_stmt chp =
      match chp with
      | Flat_chp.Stmt.Nop -> Ir.Chp.Nop
      | Assert e -> Assert (dummy_loc, of_bool_expr e)
      | Assign (v, e) ->
          let v = of_v v in
          Assign (dummy_loc, (v, Cint.sexp_of_t), of_expr e)
      | Seq ns -> Seq (dummy_loc, List.map ns ~f:of_stmt)
      | Par ns -> Par (dummy_loc, List.map ns ~f:of_stmt)
      | ReadThenAssert (c, v, _) ->
          (* TODO for now just forget about the assert *)
          Read (dummy_loc, of_c c, (of_v v, Cint.sexp_of_t))
      | Send (c, e) ->
          let c = of_c c in
          Send (dummy_loc, (c, Cint.sexp_of_t), of_expr e)
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
          | None -> Ir.Chan.create c.bitwidth Code_pos.dummy_loc)
    in
    List.iter dflow.iports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Ir.Chan.t) = of_c chp_chan ~interproc_chan in
        ());
    List.iter dflow.oports ~f:(fun (interproc_chan, chp_chan) ->
        let (_ : Ir.Chan.t) = of_c chp_chan ~interproc_chan in
        ());

    let new_var ?init bits = Ir.Var.create bits Code_pos.dummy_loc init in

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
            |> List.map ~f:(fun (i, ivar) -> Ir.Chp.read (of_c i) ivar)
            |> Ir.Chp.par
          in
          let do_sends =
            List.map exprs ~f:(fun (dst, expr) ->
                let expr =
                  expr_k_of_expr expr
                    ~of_v:(fun v -> Map.find_exn var_of_in v)
                    ~bits_of_var:(fun v -> v.bitwidth)
                in
                Ir.Chp.send (of_c dst) expr)
            |> Ir.Chp.par
          in
          Ir.Chp.loop [ do_reads; do_sends ]
      | Split (g, i, os) ->
          let tmp = new_var i.bitwidth in
          let g_var = new_var g.bitwidth in
          Ir.Chp.loop
            [
              Ir.Chp.read (of_c g) g_var;
              Ir.Chp.read (of_c i) tmp;
              Ir.Chp.select_imm
                (List.mapi os ~f:(fun idx o ->
                     let g = Ir.Expr.(eq (var g_var) (of_int idx)) in
                     let n =
                       match o with
                       | Some o -> Ir.Chp.send_var (of_c o) tmp
                       | None -> Ir.Chp.seq []
                     in
                     (g, n)))
                ~else_:None;
            ]
      | Merge (g, ins, o) ->
          let tmp = new_var o.bitwidth in
          let g_var = new_var g.bitwidth in
          Ir.Chp.loop
            [
              Ir.Chp.read (of_c g) g_var;
              Ir.Chp.select_imm
                (List.mapi ins ~f:(fun idx i ->
                     ( Ir.Expr.(eq (var g_var) (of_int idx)),
                       Ir.Chp.read (of_c i) tmp )))
                ~else_:None;
              Ir.Chp.send_var (of_c o) tmp;
            ]
      | Buff1 (dst, src, init) -> (
          assert (Int.equal dst.bitwidth src.bitwidth);
          match init with
          | Some init ->
              let tmp = new_var src.bitwidth ~init in
              Ir.Chp.loop
                [ Ir.Chp.send_var (of_c dst) tmp; Ir.Chp.read (of_c src) tmp ]
          | None ->
              let tmp = new_var src.bitwidth in
              Ir.Chp.loop
                [ Ir.Chp.read (of_c src) tmp; Ir.Chp.send_var (of_c dst) tmp ])
      | Clone (i, os) ->
          let tmp = new_var i.bitwidth in
          Ir.Chp.loop
            [
              Ir.Chp.read (of_c i) tmp;
              Ir.Chp.par
                (List.map os ~f:(fun o -> Ir.Chp.send_var (of_c o) tmp));
            ]
      | Sink i ->
          let tmp = new_var i.bitwidth in
          Ir.Chp.loop [ Ir.Chp.read (of_c i) tmp ]
    in
    Ir.Chp.Par (Code_pos.dummy_loc, List.map dflow.stmt ~f:of_stmt)
  in

  let of_mem (mem : Flat_mem.Proc.t) =
    (* init : Cint.t array; idx_bits : int; cell_bits : int; cmd_chan :
       Interproc_chan.t; read_chan : Interproc_chan.t; write_chan :
       Interproc_chan.t option; *)
    let cp = Code_pos.dummy_loc in
    let cmd_chan = of_interproc_chan mem.cmd_chan in
    let read_chan = of_interproc_chan mem.read_chan in
    let write_chan = Option.map ~f:of_interproc_chan mem.write_chan in
    let cmd = Ir.Var.create (mem.idx_bits + 1) cp None in
    let tmp = Ir.Var.create mem.cell_bits cp None in
    let mem = Ir.Mem.create mem.cell_bits cp mem.init `Mem in
    Ir.Chp.loop
      [
        Ir.Chp.read cmd_chan cmd;
        Ir.Chp.if_else
          (Eq (Const Cint.zero, BitAnd (Const Cint.one, Var cmd)))
          [
            Ir.Chp.read_mem mem
              ~idx:(LogicalRShift (Var cmd, Const Cint.one))
              ~dst:tmp;
            Ir.Chp.send_var read_chan tmp;
          ]
          (match write_chan with
          | Some write_chan ->
              [
                Ir.Chp.read write_chan tmp;
                Ir.Chp.write_mem mem
                  ~idx:(LogicalRShift (Var cmd, Const Cint.one))
                  ~value:(Var tmp);
              ]
          | None -> [ Ir.Chp.assert_ (Const Cint.zero) ]);
      ]
  in

  (* generate a big pile of parallel chp processes *)
  let chps =
    List.map t.processes ~f:(fun process ->
        match process with
        | Chp chp -> of_chp chp
        | Dflow dflow -> of_dflow dflow
        | Mem mem -> of_mem mem)
  in
  let chp = Ir.Chp.Par (dummy_loc, chps) in
  let iports = List.map t.top_iports ~f:snd |> Ir.Chan.Set.of_list in
  let oports = List.map t.top_oports ~f:snd |> Ir.Chan.Set.of_list in

  let process = { Ir.Process.inner = Chp chp; iports; oports } in

  Sim.simulate ?seed process
