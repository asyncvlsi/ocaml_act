open! Core
module CInt = Act.CInt

module Chp_exporter = struct
  module Var = Flat_program.Chp.Var
  module Chan = Flat_program.Chp.Chan

  module N = struct
    type t =
      | Nop
      | Assign of Var.t * Var.t Expr.t
      | Seq of t list
      | Par of t list
      | Read of Chan.t * Var.t
      | Send of Chan.t * Var.t Expr.t
      | DoWhile of t * Var.t Expr.t
      | SelectImm of Var.t Expr.t * t list
  end

  let rec flatten n =
    match n with
    | Flat_program.Chp.Stmt.Par ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n -> match n with N.Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n with N.Par ns -> ns | _ -> [ n ])
        in
        match ns with [] -> Nop | [ n ] -> n | ls -> N.Par ls)
    | Seq ns -> (
        let ns =
          List.map ns ~f:flatten
          |> List.filter ~f:(fun n -> match n with Nop -> false | _ -> true)
          |> List.concat_map ~f:(fun n ->
                 match n with N.Seq ns -> ns | _ -> [ n ])
        in
        match ns with [] -> Nop | [ n ] -> n | ls -> N.Seq ls)
    | SelectImm (_, guard, branches) ->
        SelectImm (guard, List.map branches ~f:flatten)
    | DoWhile (_, seq, expr) -> DoWhile (flatten seq, expr)
    | Assign (_, id, expr) -> Assign (id, expr)
    | Send (_, chan, expr) -> Send (chan, expr)
    | ReadThenAssert (_, chan, var, _) -> Read (chan, var)
    (* | WaitUntilReadReady (_, chan) -> WaitUntilReadReady chan *)
    (* | WaitUntilSendReady (_, chan) -> WaitUntilSendReady chan *)
    | Log _ | Assert _ | Nop -> Nop

  let export_proc n ~iports ~oports ~name =
    let all_vars =
      let extract_expr (e : Var.t Expr.t) = Expr.var_ids e in
      let rec extract_n n =
        match n with
        | N.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
        | Assign (var_id, expr) -> var_id :: extract_expr expr
        | Nop -> []
        | Read (_, var_id) -> [ var_id ]
        | Send (_, expr) -> extract_expr expr
        | DoWhile (n, expr) -> extract_expr expr @ extract_n n
        | SelectImm (guard, branches) ->
            extract_expr guard @ List.concat_map branches ~f:extract_n
      in
      extract_n n |> Var.Set.of_list
    in

    let all_chans =
      let rec extract_n n =
        match n with
        | N.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
        | Nop -> []
        | Assign (_, _) -> []
        | Read (chan, _) -> [ chan ]
        | Send (chan, _) -> [ chan ]
        | DoWhile (n, _) -> extract_n n
        | SelectImm (_, branches) -> List.concat_map branches ~f:extract_n
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
      let rec ee (e : Var.t Expr.t) =
        match e with
        | Expr.Add (a, b) -> [%string "(%{ee a} + %{ee b})"]
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
        | Ne (a, b) -> [%string "int(%{ee a} != %{ee b})"]
        | Lt (a, b) -> [%string "int(%{ee a} < %{ee b})"]
        | Le (a, b) -> [%string "int(%{ee a} <= %{ee b})"]
        | Gt (a, b) -> [%string "int(%{ee a} > %{ee b})"]
        | Ge (a, b) -> [%string "int(%{ee a} >= %{ee b})"]
        | Var var_id -> [%string "(%{extract_var var_id})"]
        | Clip (e, bits) -> [%string "int(%{ee e}, %{bits#Int})"]
        | Const c -> [%string "%{c#CInt}"]
      in
      let extract_expr e = ee e in
      let rec extract n =
        match n with
        | N.Par ns ->
            List.map ns ~f:(fun n -> [%string "(%{extract n})"])
            |> String.concat ~sep:", "
        | Seq ns ->
            List.map ns ~f:(fun n -> [%string "(%{extract n})"])
            |> String.concat ~sep:"; "
        | Assign (var_id, expr) ->
            [%string "%{extract_var var_id} := %{extract_expr expr}"]
        | Read (chan, var) ->
            [%string "%{extract_chan chan}?%{extract_var var}"]
        | Send (chan, expr) ->
            [%string "%{extract_chan chan}!(%{extract_expr expr})"]
        | DoWhile (n, guard) ->
            [%string " *[ %{extract n} <- bool(%{extract_expr guard}) ] "]
        | SelectImm (guard, branches) ->
            let branches =
              List.mapi branches ~f:(fun idx n ->
                  [%string
                    "%{extract_expr guard} = %{Int.pow 2 idx#Int} -> %{extract \
                     n}"])
              |> String.concat ~sep:" [] "
            in
            [%string "[%{branches}]"]
        | Nop -> " [true] "
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

  let export (chp_proc : Flat_program.Chp.Proc.t) ~name =
    let n = flatten chp_proc.stmt in
    let io_ports = List.map ~f:fst (chp_proc.iports @ chp_proc.oports) in
    let s =
      export_proc n ~name ~iports:chp_proc.iports ~oports:chp_proc.oports
    in
    (s, (name, io_ports))
end

module Mem_exporter = struct
  let export (mem : Flat_program.Mem_proc.t) ~name =
    let init = mem.init in
    let vars_inits =
      Array.mapi init ~f:(fun i vl -> [%string "v[%{i#Int}] := %{vl#CInt};"])
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

let export_program (prog : Flat_program.Program.t) =
  (* First export each process *)
  let procs_decls, procs =
    List.mapi prog.processes ~f:(fun i proc ->
        let name = [%string "proc_%{i#Int}"] in
        match proc.k with
        | Chp chp_proc -> Chp_exporter.export chp_proc ~name
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
(* String.concat ~sep:"\n\n" [ header; procs; top_level_proc ] *)
