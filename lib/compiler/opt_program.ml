open! Core

module Process = struct
  type t =
    | Chp of Flat_chp.Proc.t
    | Dflow of Flat_dflow.Proc.t
    | Mem of Flat_mem.Proc.t
  [@@deriving sexp_of]
end

type t = {
  processes : Process.t list;
  top_iports : (Interproc_chan.t * Ir_chan.U.t) list;
  top_oports : (Interproc_chan.t * Ir_chan.U.t) list;
}
[@@deriving sexp_of]

let of_prog (prog : Program.t) =
  (* First export each process *)
  {
    processes =
      List.map prog.processes ~f:(fun proc ->
          match proc.k with
          | Chp chp_proc -> (
              match chp_proc.dflowable with
              | false -> Process.Chp chp_proc
              | true -> Dflow (Flat_dflow.of_chp chp_proc))
          | Mem mem_proc -> Mem mem_proc);
    top_iports = prog.top_iports;
    top_oports = prog.top_oports;
  }

module Chp_exporter = struct
  module Var = Flat_chp.Var
  module Chan = Flat_chp.Chan

  let export_proc n ~iports ~oports ~name =
    let all_vars =
      let extract_expr (e : Var.t F_expr.t) = F_expr.var_ids e in
      let rec extract_n n =
        match n with
        | Flat_chp.Stmt.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
        | Assign (var_id, expr) -> var_id :: extract_expr expr
        | Nop | Assert _ -> []
        | ReadThenAssert (_, var_id, _) -> [ var_id ]
        | Send (_, expr) -> extract_expr expr
        | DoWhile (n, expr) -> extract_expr expr @ extract_n n
        | SelectImm (guards, branches) ->
            List.concat_map ~f:extract_expr guards
            @ List.concat_map branches ~f:extract_n
        | Nondeterm_select branches ->
            List.concat_map branches ~f:(fun (_, stmt) -> extract_n stmt)
      in
      extract_n n |> Var.Set.of_list
    in

    let all_chans =
      let rec extract_n n =
        match n with
        | Flat_chp.Stmt.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
        | Nop | Assert _ -> []
        | Assign (_, _) -> []
        | ReadThenAssert (chan, _, _) -> [ chan ]
        | Send (chan, _) -> [ chan ]
        | DoWhile (n, _) -> extract_n n
        | SelectImm (_, branches) -> List.concat_map branches ~f:extract_n
        | Nondeterm_select branches ->
            List.concat_map branches ~f:(fun (probe, stmt) ->
                let probe = match probe with Read chan | Send chan -> chan in
                probe :: extract_n stmt)
      in
      extract_n n |> Chan.Set.of_list
    in

    let all_vars =
      Set.to_list all_vars
      |> List.mapi ~f:(fun i var -> (var, sprintf "v%d" i))
      |> Var.Map.of_alist_exn
    in
    let all_chans =
      Set.to_list all_chans
      |> List.mapi ~f:(fun i var -> (var, sprintf "C%d" i))
      |> Chan.Map.of_alist_exn
    in

    let decl_iports =
      iports |> List.map ~f:snd
      |> List.map ~f:(fun port ->
             let bitwidth = port.Chan.bitwidth in
             let name = Map.find_exn all_chans port in
             sprintf "chan!(int<%d>) %s" bitwidth name)
    in
    let decl_oports =
      oports |> List.map ~f:snd
      |> List.map ~f:(fun port ->
             let bitwidth = port.Chan.bitwidth in
             let name = Map.find_exn all_chans port in
             sprintf "chan?(int<%d>) %s" bitwidth name)
    in
    let decl_io_ports = decl_iports @ decl_oports |> String.concat ~sep:"; " in
    let not_io_chans =
      let io_ports = oports @ iports |> List.map ~f:snd |> Chan.Set.of_list in
      Set.diff (Map.key_set all_chans) io_ports
    in
    let decl_not_io_chans =
      Set.to_list not_io_chans
      |> List.map ~f:(fun chan ->
             let bitwidth = chan.bitwidth in
             let name = Map.find_exn all_chans chan in
             sprintf "  chan(int<%d>) %s;" bitwidth name)
      |> String.concat ~sep:"\n"
    in
    let decl_vars =
      Map.keys all_vars
      |> List.map ~f:(fun var ->
             let bitwidth = var.bitwidth in
             let name = Map.find_exn all_vars var in
             sprintf "  int<%d> %s;" bitwidth name)
    in
    let decl_vars = String.concat decl_vars ~sep:"\n" in

    let chp_code =
      let extract_var var = Map.find_exn all_vars var in
      let extract_chan chan = Map.find_exn all_chans chan in
      let rec ee (e : Var.t F_expr.t) =
        match e with
        | F_expr.Add (a, b) -> [%string "(%{ee a} + %{ee b})"]
        | Sub_no_wrap (a, b) -> [%string "(%{ee a} - %{ee b})"]
        | Mul (a, b) -> [%string "(%{ee a} * %{ee b})"]
        | Div (a, b) -> [%string "(%{ee a} / %{ee b})"]
        | Mod (a, b) -> [%string "(%{ee a} % %{ee b})"]
        | LShift (a, b) -> [%string "(%{ee a} << %{ee b})"]
        | RShift (a, b) -> [%string "(%{ee a} >> %{ee b})"]
        | BitAnd (a, b) -> [%string "(%{ee a} & %{ee b})"]
        | BitOr (a, b) -> [%string "(%{ee a} | %{ee b})"]
        | BitXor (a, b) -> [%string "(%{ee a} ^ %{ee b})"]
        | Eq (a, b) -> [%string "int(%{ee a} = %{ee b})"]
        | Eq0 a -> [%string "int(%{ee a} = 0)"]
        | Ne (a, b) -> [%string "int(%{ee a} != %{ee b})"]
        | Lt (a, b) -> [%string "int(%{ee a} < %{ee b})"]
        | Le (a, b) -> [%string "int(%{ee a} <= %{ee b})"]
        | Gt (a, b) -> [%string "int(%{ee a} > %{ee b})"]
        | Ge (a, b) -> [%string "int(%{ee a} >= %{ee b})"]
        | Var var_id -> [%string "(%{extract_var var_id})"]
        | Clip (e, bits) -> [%string "int(%{ee e}, %{bits#Int})"]
        | Const c -> [%string "%{c#Cint}"]
        | Concat es ->
            let s =
              List.rev es
              |> List.map ~f:(fun (e, bits) ->
                     [%string "int(%{ee e}, %{bits#Int})"])
              |> String.concat ~sep:", "
            in
            [%string "{ %{s} }"]
        | Log2OneHot e -> [%string "( log2_one_hot(%{ee e}) )"]
      in
      let extract_expr e = ee e in
      let rec extract n =
        match n with
        | Flat_chp.Stmt.Par ns ->
            List.map ns ~f:(fun n -> [%string "(%{extract n})"])
            |> String.concat ~sep:", "
        | Seq ns ->
            List.map ns ~f:(fun n -> [%string "(%{extract n})"])
            |> String.concat ~sep:"; "
        | Assign (var_id, expr) ->
            [%string "%{extract_var var_id} := %{extract_expr expr}"]
        | ReadThenAssert (chan, var, _) ->
            [%string "%{extract_chan chan}?%{extract_var var}"]
        | Send (chan, expr) ->
            [%string "%{extract_chan chan}!(%{extract_expr expr})"]
        | DoWhile (n, guard) ->
            [%string " *[ %{extract n} <- bool(%{extract_expr guard}) ] "]
        | SelectImm (guards, branches) ->
            let branches =
              List.zip_exn guards branches
              |> List.map ~f:(fun (g, n) ->
                     [%string "bool(%{extract_expr g}) -> %{extract n}"])
              |> String.concat ~sep:" [] "
            in
            [%string "[%{branches}]"]
        | Nondeterm_select branches ->
            let branches =
              List.map branches ~f:(fun (probe, n) ->
                  let chan = match probe with Send chan | Read chan -> chan in
                  [%string "#%{extract_chan chan} -> %{extract n}"])
              |> String.concat ~sep:" [] "
            in
            [%string "[| %{branches} |]"]
        | Nop | Assert _ -> " [true] "
      in
      extract n
    in

    [%string
      "defproc %{name}(%{decl_io_ports}) {\n\
       %{decl_not_io_chans}\n\
       %{decl_vars}\n\
       chp {\n\
       %{chp_code}\n\
       }\n\
       }"]

  let export (chp_proc : Flat_chp.Proc.t) ~name =
    let stmt = Flat_chp.Stmt.flatten chp_proc.stmt in
    let io_ports = List.map ~f:fst (chp_proc.iports @ chp_proc.oports) in
    let s =
      export_proc stmt ~name ~iports:chp_proc.iports ~oports:chp_proc.oports
    in
    (s, (name, io_ports))
end

module Dflow_exporter = struct
  let export_proc proc ~name =
    let { Flat_dflow.Proc.stmt = ns; iports; oports } = proc in
    let all_ids = Flat_dflow.var_ids proc |> Set.to_list in
    let decl_vars =
      List.map all_ids ~f:(fun id ->
          [%string "  chan(int<%{id.bitwidth#Int}>) v%{id.id#Int};"])
      |> String.concat ~sep:"\n"
    in
    let stmts =
      let rec ee (e : Flat_dflow.Var.t F_expr.t) =
        match e with
        | F_expr.Add (a, b) -> [%string "(%{ee a} + %{ee b})"]
        | Sub_no_wrap (a, b) -> [%string "(%{ee a} - %{ee b})"]
        | Mul (a, b) -> [%string "(%{ee a} * %{ee b})"]
        | Div (a, b) -> [%string "(%{ee a} / %{ee b})"]
        | Mod (a, b) -> [%string "(%{ee a} % %{ee b})"]
        | LShift (a, b) -> [%string "(%{ee a} << %{ee b})"]
        | RShift (a, b) -> [%string "(%{ee a} >> %{ee b})"]
        | BitAnd (a, b) -> [%string "(%{ee a} & %{ee b})"]
        | BitOr (a, b) -> [%string "(%{ee a} | %{ee b})"]
        | BitXor (a, b) -> [%string "(%{ee a} ^ %{ee b})"]
        | Eq (a, b) -> [%string "int(%{ee a} = %{ee b})"]
        | Eq0 a -> [%string "int(%{ee a} = 0)"]
        | Ne (a, b) -> [%string "int(%{ee a} != %{ee b})"]
        | Lt (a, b) -> [%string "int(%{ee a} < %{ee b})"]
        | Le (a, b) -> [%string "int(%{ee a} <= %{ee b})"]
        | Gt (a, b) -> [%string "int(%{ee a} > %{ee b})"]
        | Ge (a, b) -> [%string "int(%{ee a} >= %{ee b})"]
        | Var v -> [%string "(v%{v.id#Int})"]
        | Clip (e, bits) -> [%string "int(%{ee e}, %{bits#Int})"]
        | Const c -> [%string "%{c#Cint}"]
        | Concat es ->
            let s =
              List.rev es
              |> List.map ~f:(fun (e, bits) ->
                     [%string "int(%{ee e}, %{bits#Int})"])
              |> String.concat ~sep:", "
            in
            [%string "{ %{s} }"]
        | Log2OneHot e -> [%string "( log2_one_hot(%{ee e}) )"]
      in
      let vv (v : Flat_dflow.Var.t) = [%string "v%{v.id#Int}"] in
      let vvo o = match o with Some v -> vv v | None -> "*" in
      List.map ns ~f:(fun n ->
          match n with
          | MultiAssign fblock ->
              let open Flat_dflow in
              let es = FBlock.expr_list fblock in
              assert (not (List.is_empty es));
              let ins_set = FBlock.ins fblock |> Var.Set.of_list in
              let e_ins_set =
                List.concat_map es ~f:(fun (_, e) -> F_expr.var_ids e)
                |> Var.Set.of_list
              in
              assert (Set.diff e_ins_set ins_set |> Set.is_empty);
              if Set.equal ins_set e_ins_set && List.length es |> Int.equal 1
              then
                let dst, e = List.hd_exn es in
                [%string "  v%{dst.id#Int} <- %{ee e};"]
              else
                let extra_ins = Set.diff ins_set e_ins_set in
                let es =
                  match es with
                  | [] -> failwith "unreachable"
                  | (dst, e) :: es ->
                      let extra_e =
                        Set.to_list extra_ins
                        |> List.map ~f:(fun v ->
                               F_expr.(BitAnd (Const Cint.zero, Var v)))
                        |> List.reduce ~f:(fun a b -> F_expr.BitAnd (a, b))
                      in
                      let e =
                        match extra_e with
                        | Some extra_e -> F_expr.BitOr (e, extra_e)
                        | None -> e
                      in
                      (dst, e) :: es
                in
                let exprs =
                  List.map es ~f:(fun (dst, e) ->
                      [%string "  v%{dst.id#Int} <- %{ee e};"])
                  |> String.concat ~sep:"\n  "
                in
                [%string "  dataflow_cluser {\n  %{exprs}\n  };"]
          | Split (g, v, os) ->
              let os = List.map os ~f:vvo |> String.concat ~sep:", " in
              [%string "  { v%{g.id#Int} } v%{v.id#Int} -> %{os};"]
          | Merge (g, ins, v) ->
              let ins =
                List.map ins ~f:(fun i -> [%string "v%{i.id#Int}"])
                |> String.concat ~sep:", "
              in
              [%string "  { v%{g.id#Int} } %{ins} -> v%{v.id#Int};"]
          | Clone (src, dsts) ->
              (* TODO is this right? *)
              List.map dsts ~f:(fun dst ->
                  [%string "  v%{dst.id#Int} <- v%{src.id#Int};"])
              |> String.concat ~sep:"   "
          | Sink _ ->
              (* TODO is this right? *)
              ""
          | Buff1 (dst, src, init) -> (
              match init with
              | Some init ->
                  [%string
                    "  v%{src.id#Int} -> [1,%{init#Cint}] v%{dst.id#Int};"]
              | None -> [%string "  v%{src.id#Int} -> [1] v%{dst.id#Int};"]))
      |> String.concat ~sep:"\n"
    in

    let decl_iports =
      iports |> List.map ~f:snd
      |> List.map ~f:(fun port ->
             let bitwidth = port.bitwidth in
             [%string "chan!(int<%{bitwidth#Int}>) iport%{port.id#Int}"])
    in
    let decl_oports =
      oports |> List.map ~f:snd
      |> List.map ~f:(fun port ->
             let bitwidth = port.bitwidth in
             [%string "chan?(int<%{bitwidth#Int}>) oport%{port.id#Int}"])
    in
    let decl_io_ports = decl_iports @ decl_oports |> String.concat ~sep:"; " in
    let connect_ports =
      List.map iports ~f:(fun (_, port) ->
          [%string "iport%{port.id#Int} -> v%{port.id#Int};"])
      @ List.map oports ~f:(fun (_, port) ->
            [%string "v%{port.id#Int} -> oport%{port.id#Int};"])
      |> String.concat ~sep:"\n"
    in
    [%string
      "defproc %{name}(%{decl_io_ports}) {\n\
       %{decl_vars}\n\
       dataflow {\n\
       %{stmts}\n\
       %{connect_ports}\n\
       }\n\
       }"]

  let export (dflow_proc : Flat_dflow.Proc.t) ~name =
    let s = export_proc dflow_proc ~name in
    let io_ports = List.map ~f:fst (dflow_proc.iports @ dflow_proc.oports) in
    (s, (name, io_ports))
end

module Mem_exporter = struct
  let export (mem : Flat_mem.Proc.t) ~name =
    let init = mem.init in
    let vars_inits =
      Array.mapi init ~f:(fun i vl -> [%string "v[%{i#Int}] := %{vl#Cint};"])
      |> Array.to_list |> String.concat ~sep:""
    in
    let write_chan_decl, write_branch_body, write_chan =
      match mem.write_chan with
      | Some write_chan ->
          ( "; chan?(int<%{mem.cell_bits#Int}>) write_chan",
            "; write_chan?tmp;\n\
             [\n\
             ([]: j:%{Array.length init#Int}:  (cmd >> 1) = j   ->  v[j] := \
             tmp  )\n\
             ]",
            [ write_chan ] )
      | None -> ("", "", [])
    in
    let s =
      [%string
        "defproc %{name}(chan?(int<%{mem.idx_bits + 1#Int}>>) cmd_chan; \
         chan?(int<%{mem.cell_bits#Int}>>) read_chan%{write_chan_decl}) {\n\n\
         int<%{mem.cell_bits#Int}> v[%{Array.length init#Int}];\n\
         int<%{mem.idx_bits + 1#Int}> cmd;\n\
         int<%{mem.idx_bits + 1#Int}> tmp;\n\
         chp {\n\
         %{vars_inits}\n\
         *[\n\
         cmd_chan?cmd;\n\
         [ cmd & 1 = 0 ->\n\
         [\n\
         ([]: j:%{Array.length init#Int}:  (cmd >> 1) = j   ->  tmp := v[j]  )\n\
         ];\n\
         read_chan!tmp\n\
         [] cmd & 1 = 1 -> \n\
         skip\n\
         %{write_branch_body}\n\
         ]\n\
         ]\n\
         }\n\
         }"]
    in

    let io_ports = [ mem.cmd_chan; mem.read_chan ] @ write_chan in
    (s, (name, io_ports))
end

let export (prog : t) =
  (* First export each process *)
  let procs_decls, procs =
    List.mapi prog.processes ~f:(fun i proc ->
        let name = [%string "proc_%{i#Int}"] in
        match proc with
        | Chp chp_proc -> Chp_exporter.export chp_proc ~name
        | Dflow dflow_proc -> Dflow_exporter.export dflow_proc ~name
        | Mem mem_proc -> Mem_exporter.export mem_proc ~name)
    |> List.unzip
  in
  let procs_decls = String.concat procs_decls ~sep:"\n\n" in
  let interproc_chans =
    List.concat_map procs ~f:snd
    |> Interproc_chan.Set.of_list |> Set.to_list
    |> List.mapi ~f:(fun i c -> (c, sprintf "c%d" i))
    |> Interproc_chan.Map.of_alist_exn
  in
  let decl_chans =
    Map.to_alist interproc_chans
    |> List.map ~f:(fun (chan, chan_name) ->
           sprintf "  chan(int<%d>) %s;" chan.bitwidth chan_name)
    |> String.concat ~sep:"\n"
  in
  let instantiate_procs =
    List.map procs ~f:(fun (name, io_ports) ->
        let io_ports =
          List.map io_ports ~f:(fun port -> Map.find_exn interproc_chans port)
          |> String.concat ~sep:","
        in
        [%string "%{name} %{name}_ (%{io_ports});"])
    |> String.concat ~sep:"\n"
  in
  let decl_user_io_ports =
    List.mapi prog.top_iports ~f:(fun i (port, _) ->
        [%string "chan?(int<%{port.bitwidth#Int}>) user_i%{i#Int}"])
    @ List.mapi prog.top_oports ~f:(fun i (port, _) ->
          [%string "chan!(int<%{port.bitwidth#Int}>) user_o%{i#Int}"])
    |> String.concat ~sep:";"
  in

  let connect_user_ports =
    List.mapi prog.top_iports ~f:(fun i (port, _) ->
        let port_name = Map.find_exn interproc_chans port in
        [%string "%{port_name} = user_i%{i#Int};"])
    @ List.mapi prog.top_oports ~f:(fun i (port, _) ->
          let port_name = Map.find_exn interproc_chans port in
          [%string "%{port_name} = user_o%{i#Int};"])
    |> String.concat ~sep:"\n"
  in

  [%string
    "%{procs_decls}\n\n\n\
     defproc main(%{decl_user_io_ports}) {\n\n\
     %{decl_chans}\n\
     %{instantiate_procs}\n\
     %{connect_user_ports}\n\
     }"]
