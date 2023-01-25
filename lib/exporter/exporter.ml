open! Core
open! Act
module Ir = Internal_rep

module N = struct
  type t =
    | Nop
    | Assign of Ir.Var.U.t * Ir.Expr.U.t
    | Seq of t list
    | Par of t list
    | Read of Ir.Chan.U.t * Ir.Var.U.t
    | Send of Ir.Chan.U.t * Ir.Expr.U.t
    | Loop of t
    | WhileLoop of bool Ir.Expr.t * t
    | SelectImm of (bool Ir.Expr.t * t) list * t option
    | ReadUGMem of Ir.Mem.t * CInt.t Ir.Expr.t * Ir.Var.U.t
    | WriteUGMem of Ir.Mem.t * CInt.t Ir.Expr.t * Ir.Expr.U.t
    | WaitUntilReadReady of Ir.Chan.U.t
    | WaitUntilSendReady of Ir.Chan.U.t
end

type t = string

let rec flatten n =
  match n with
  | Ir.N.Par (_, ns) -> (
      let ns =
        List.map ns ~f:flatten
        |> List.filter ~f:(fun n -> match n with N.Nop -> false | _ -> true)
        |> List.concat_map ~f:(fun n ->
               match n with N.Par ns -> ns | _ -> [ n ])
      in
      match ns with [] -> Nop | [ n ] -> n | ls -> N.Par ls)
  | Seq (_, ns) -> (
      let ns =
        List.map ns ~f:flatten
        |> List.filter ~f:(fun n -> match n with Nop -> false | _ -> true)
        |> List.concat_map ~f:(fun n ->
               match n with N.Seq ns -> ns | _ -> [ n ])
      in
      match ns with [] -> Nop | [ n ] -> n | ls -> N.Seq ls)
  | Loop (_, n) -> Loop (flatten n)
  | SelectImm (_, branches, else_) ->
      SelectImm
        ( List.map branches ~f:(fun (guard, n) -> (guard, flatten n)),
          Option.map else_ ~f:flatten )
  | WhileLoop (_, expr, seq) -> WhileLoop (expr, flatten seq)
  | Assign (_, id, expr) -> Assign (id, expr)
  | Send (_, chan, expr) -> Send (chan, expr)
  | Read (_, chan, var) -> Read (chan, var)
  | ReadUGMem (_, mem, idx, dst) -> ReadUGMem (mem, idx, dst)
  | WriteUGMem (_, mem, idx, value) -> WriteUGMem (mem, idx, value)
  | WaitUntilReadReady (_, chan) -> WaitUntilReadReady chan
  | WaitUntilSendReady (_, chan) -> WaitUntilSendReady chan
  | Log _ | Log1 _ | Assert _ -> Nop

let create ir ~user_sendable_ports ~user_readable_ports =
  let ir = Ir.N.unwrap ir |> flatten in
  let user_sendable_ports =
    List.map user_sendable_ports ~f:Ir.Chan.unwrap_wu |> Ir.Chan.U.Set.of_list
  in
  let user_readable_ports =
    List.map user_readable_ports ~f:Ir.Chan.unwrap_ru |> Ir.Chan.U.Set.of_list
  in
  assert (Set.inter user_readable_ports user_sendable_ports |> Set.is_empty);

  let all_vars =
    let rec extract_expr (e : Ir.Expr.K.t) =
      match e with
      | Ir.Expr.K.Add (a, b)
      | Sub_no_wrap (a, b)
      | Sub_wrap (a, b, _)
      | Mul (a, b)
      | Div (a, b)
      | Mod (a, b)
      | LShift (a, b)
      | LogicalRShift (a, b)
      | BitAnd (a, b)
      | BitOr (a, b)
      | BitXor (a, b)
      | Eq (a, b)
      | Ne (a, b)
      | Lt (a, b)
      | Le (a, b)
      | Gt (a, b)
      | Ge (a, b) ->
          extract_expr a @ extract_expr b
      | Var var_id -> [ var_id ]
      | Clip (e, _) -> extract_expr e
      | Const _ -> []
      | With_assert_log (a, v, l, _) ->
          extract_expr a @ extract_expr v @ extract_expr l
      | With_assert_log_fn (a, _, v) -> extract_expr a @ extract_expr v
    in
    let extract_expr e = extract_expr e.Ir.Expr.k in
    let rec extract_n n =
      match n with
      | N.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
      | Loop n -> extract_n n
      | Assign (var_id, expr) -> var_id :: extract_expr expr
      | Nop -> []
      | Read (_, var_id) -> [ var_id ]
      | Send (_, expr) -> extract_expr expr
      | WhileLoop (expr, n) -> extract_expr expr @ extract_n n
      | SelectImm (branches, else_) ->
          List.concat_map branches ~f:(fun (expr, n) ->
              extract_expr expr @ extract_n n)
          @ (Option.map else_ ~f:extract_n |> Option.value ~default:[])
      | ReadUGMem (_, idx, dst) -> dst :: extract_expr idx
      | WriteUGMem (_, idx, value) -> extract_expr idx @ extract_expr value
      | WaitUntilReadReady _ -> []
      | WaitUntilSendReady _ -> []
    in
    extract_n ir |> Ir.Var.U.Set.of_list
  in

  let all_chans =
    let rec extract_n n =
      match n with
      | N.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
      | Loop n -> extract_n n
      | Nop -> []
      | Assign (_, _) -> []
      | Read (chan, _) -> [ chan ]
      | Send (chan, _) -> [ chan ]
      | WhileLoop (_, n) -> extract_n n
      | SelectImm (branches, else_) ->
          List.concat_map branches ~f:(fun (_, n) -> extract_n n)
          @ (Option.map else_ ~f:extract_n |> Option.value ~default:[])
      | ReadUGMem (_, _, _) -> []
      | WriteUGMem (_, _, _) -> []
      | WaitUntilReadReady chan -> [ chan ]
      | WaitUntilSendReady chan -> [ chan ]
    in
    extract_n ir |> Ir.Chan.U.Set.of_list
    |> Set.union user_sendable_ports
    |> Set.union user_readable_ports
  in

  let all_mems =
    let rec extract_n n =
      match n with
      | N.Par ns | Seq ns -> List.concat_map ns ~f:extract_n
      | Loop n -> extract_n n
      | Nop -> []
      | Assign (_, _) -> []
      | Read (_, _) -> []
      | Send (_, _) -> []
      | WhileLoop (_, n) -> extract_n n
      | SelectImm (branches, else_) ->
          List.concat_map branches ~f:(fun (_, n) -> extract_n n)
          @ (Option.map else_ ~f:extract_n |> Option.value ~default:[])
      | ReadUGMem (mem, _, _) -> [ mem ]
      | WriteUGMem (mem, _, _) -> [ mem ]
      | WaitUntilReadReady _ -> []
      | WaitUntilSendReady _ -> []
    in
    extract_n ir |> Ir.Mem.Set.of_list
  in

  let all_vars =
    Set.to_list all_vars
    |> List.mapi ~f:(fun i var -> (var, sprintf "v%d" i))
    |> Ir.Var.U.Map.of_alist_exn
  in
  let all_chans =
    Set.to_list all_chans
    |> List.mapi ~f:(fun i var -> (var, sprintf "C%d" i))
    |> Ir.Chan.U.Map.of_alist_exn
  in
  let all_mems =
    Set.to_list all_mems
    |> List.mapi ~f:(fun i var -> (var, sprintf "M%d" i))
    |> Ir.Mem.Map.of_alist_exn
  in

  let proc_name = "my_proc" in
  let decl_user_readable_ports =
    Set.to_list user_readable_ports
    |> List.map ~f:(fun port ->
           let bitwidth =
             match Ir.DType.layout port.d.dtype with
             | Bits_fixed bitwidth -> bitwidth
           in
           let name = Map.find_exn all_chans port in
           sprintf "chan!(int<%d>) %s" bitwidth name)
  in
  let decl_user_sendable_ports =
    Set.to_list user_sendable_ports
    |> List.map ~f:(fun port ->
           let bitwidth =
             match Ir.DType.layout port.d.dtype with
             | Bits_fixed bitwidth -> bitwidth
           in
           let name = Map.find_exn all_chans port in
           sprintf "chan?(int<%d>) %s" bitwidth name)
  in
  let decl_io_ports =
    decl_user_readable_ports @ decl_user_sendable_ports
    |> String.concat ~sep:"; "
  in
  let not_io_chans =
    Set.diff (Map.key_set all_chans)
      (Set.union user_readable_ports user_sendable_ports)
  in
  let decl_not_io_chans =
    Set.to_list not_io_chans
    |> List.map ~f:(fun chan ->
           let bitwidth =
             match Ir.DType.layout chan.d.dtype with
             | Bits_fixed bitwidth -> bitwidth
           in
           let name = Map.find_exn all_chans chan in
           sprintf "  chan(int<%d>) %s;" bitwidth name)
    |> String.concat ~sep:"\n"
  in
  let decl_vars, init_vars =
    Map.keys all_vars
    |> List.map ~f:(fun var ->
           let bitwidth =
             match Ir.DType.layout var.d.dtype with
             | Bits_fixed bitwidth -> bitwidth
           in
           let name = Map.find_exn all_vars var in
           let decl = sprintf "  int<%d> %s;" bitwidth name in
           let init =
             Option.map var.d.init ~f:(fun init ->
                 let init =
                   Ir.DType.cint_of_value var.d.dtype init |> CInt.to_string
                 in
                 sprintf "%s := %s;" name init)
           in
           (decl, init))
    |> List.unzip
  in
  let decl_vars = String.concat decl_vars ~sep:"\n" in
  let init_vars = List.filter_opt init_vars |> String.concat ~sep:"\n    " in
  let decl_mems, init_mems =
    Map.keys all_mems
    |> List.map ~f:(fun mem ->
           let bitwidth =
             match Ir.DType.layout mem.d.dtype with
             | Bits_fixed bitwidth -> bitwidth
           in
           let name = Map.find_exn all_mems mem in
           let len = Array.length mem.d.init in
           let nlen = CInt.bitwidth (CInt.of_int (len - 1)) in
           let decl = sprintf "  ram<%d, %d, %d> %s;" len nlen bitwidth name in
           let init =
             Array.mapi mem.d.init ~f:(fun idx init ->
                 let init =
                   Ir.DType.cint_of_value mem.d.dtype init |> CInt.to_string
                 in
                 sprintf "(%s.Op!1, %s.Idx!%d, %s.WValue!%s);" name name idx
                   name init)
             |> Array.to_list |> String.concat ~sep:" "
           in
           (decl, init))
    |> List.unzip
  in
  let decl_mems = String.concat decl_mems ~sep:" " in
  let init_mems = String.concat init_mems ~sep:"\n    " in

  let chp_code =
    let extract_var var = Map.find_exn all_vars var in
    let extract_chan chan = Map.find_exn all_chans chan in
    let extract_mem mem = Map.find_exn all_mems mem in
    let rec ee (e : Ir.Expr.K.t) =
      match e with
      | Ir.Expr.K.Add (a, b) -> [%string "(%{ee a} + %{ee b})"]
      | Sub_no_wrap (a, b) -> [%string "(%{ee a} - %{ee b})"]
      | Sub_wrap (a, b, bits) ->
          [%string "int(int(%{ee a}, %{bits+1#Int}) - %{ee b}, %{bits#Int})"]
      | Mul (a, b) -> [%string "(%{ee a} * %{ee b})"]
      | Div (a, b) -> [%string "(%{ee a} / %{ee b})"]
      | Mod (a, b) -> [%string "(%{ee a} % %{ee b})"]
      | LShift (a, b) -> [%string "(%{ee a} << %{ee b})"]
      | LogicalRShift (a, b) -> [%string "(%{ee a} >> %{ee b})"]
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
      | With_assert_log (_, val_expr, _, _) ->
          (* TODO export the assert as well? *)
          ee val_expr
      | With_assert_log_fn (_, _, val_expr) ->
          (* TODO export the assert as well? *)
          ee val_expr
    in
    let extract_expr e = ee e.Ir.Expr.k in
    let rec extract n =
      match n with
      | N.Par ns ->
          List.map ns ~f:(fun n -> [%string "(%{extract n})"])
          |> String.concat ~sep:", "
      | Seq ns ->
          List.map ns ~f:(fun n -> [%string "(%{extract n})"])
          |> String.concat ~sep:"; "
      | Loop n -> [%string " *[%{extract n}] "]
      | Assign (var_id, expr) ->
          [%string "%{extract_var var_id} := %{extract_expr expr}"]
      | Read (chan, var) -> [%string "%{extract_chan chan}?%{extract_var var}"]
      | Send (chan, expr) ->
          [%string "%{extract_chan chan}!(%{extract_expr expr})"]
      | WhileLoop (guard, n) ->
          [%string " *[ bool(%{extract_expr guard}) -> %{extract n} ] "]
      | SelectImm (branches, else_) ->
          let branches =
            List.map branches ~f:(fun (expr, n) ->
                [%string "bool(%{extract_expr expr}) -> %{extract n}"])
            @ (Option.map else_ ~f:(fun else_ ->
                   [ [%string "else -> %{extract else_}"] ])
              |> Option.value ~default:[])
            |> String.concat ~sep:" [] "
          in
          [%string "[%{branches}]"]
      | ReadUGMem (mem, idx, dst) ->
          let mem = extract_mem mem in
          [%string
            "(%{mem}.Op!0, %{mem}.Idx!(%{extract_expr idx}), \
             %{mem}.RValue?%{extract_var dst})"]
      | WriteUGMem (mem, idx, value) ->
          let mem = extract_mem mem in
          [%string
            "(%{mem}.Op!1, %{mem}.Idx!(%{extract_expr idx}), \
             %{mem}.WValue!(%{extract_expr value}))"]
      | WaitUntilReadReady chan | WaitUntilSendReady chan ->
          [%string "[ #%{extract_chan chan}]"]
      | Nop -> " [true] "
    in
    extract ir
  in
  let mem_proc_decl =
    let mem_proc =
      [%string
        "template<pint N; pint LN;  pint W> defproc ram(chan?(int<1>) Op; \
         chan?(int<LN>) Idx; chan?(int<W>) WValue; chan!(int<W>) RValue) {\n\
         int<W> mem[N]; int<1> op; int<LN> idx; int<W> tmp;\n\
         chp { *[ Op?op, Idx?idx;\n\
         [ op = 0 -> [ ([] i:N: idx = i -> RValue!(mem[i]) ) ]\n\
         []  op = 1 -> WValue?tmp; [ ([] i:N: idx = i -> mem[i] := tmp ) ] ] ]\n\
         } }\n\n"]
    in
    if Map.is_empty all_mems then "" else mem_proc
  in
  let s =
    [%string
      "%{mem_proc_decl}defproc %{proc_name}(%{decl_io_ports}) {\n\
       %{decl_not_io_chans}\n\
       %{decl_vars}\n\
       %{decl_mems}\n\n\
       chp {\n\
       %{init_vars}\n\
       %{init_mems}\n\
       %{chp_code}\n\
       }\n\
       }"]
  in
  printf "%s" s;
  s
