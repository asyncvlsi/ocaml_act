open! Core

module T = struct
  type t =
    | Nop
    | Assign of Code_pos.t * Var.Ir.U.t * Expr.Ir.U.t
    | Log of Code_pos.t * string
    | Log1 of Code_pos.t * Expr.Ir.U.t * (Any.t -> string)
    | Assert of Code_pos.t * Cbool0.t Expr.Ir.t
    | Seq of Code_pos.t * t list
    | Par of Code_pos.t * t list
    | Read of Code_pos.t * Chan.Ir.U.t * Var.Ir.U.t
    | Send of Code_pos.t * Chan.Ir.U.t * Expr.Ir.U.t
    | Loop of Code_pos.t * t
    | WhileLoop of Code_pos.t * Cbool0.t Expr.Ir.t * t
    | DoWhile of Code_pos.t * t * Cbool0.t Expr.Ir.t
    | SelectImm of Code_pos.t * (Cbool0.t Expr.Ir.t * t) list * t option
    | ReadUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Var.Ir.U.t
    | WriteUGMem of Code_pos.t * Mem.Ir.t * Cint0.t Expr.Ir.t * Expr.Ir.U.t
    | WaitUntilReadReady of Code_pos.t * Chan.Ir.U.t
    | WaitUntilSendReady of Code_pos.t * Chan.Ir.U.t
  [@@deriving sexp_of]
end

module No_width_checks = struct
  let assign ?loc var_id expr =
    T.Assign
      (Code_pos.value_or_psite loc, Var.Ir.untype var_id, Expr.Ir.untype expr)

  let read ?loc chan_id var_id =
    T.Read (Code_pos.value_or_psite loc, chan_id, Var.Ir.untype var_id)

  let send ?loc chan_id expr =
    T.Send (Code_pos.value_or_psite loc, chan_id, Expr.Ir.untype expr)
end

module Ir = struct
  include T
  module No_width_checks = No_width_checks

  let unwrap t = t
  let wrap t = t
end

let fail_if_layout_does_not_fit_dtype err_msg layout dtype =
  match Dtype.Ir.fits_into_dtype layout ~into:dtype with
  | true -> ()
  | false ->
      failwith
        (sprintf err_msg
           (Layout.sexp_of_t layout |> Sexp.to_string)
           (Dtype.Ir.layout dtype |> Layout.sexp_of_t |> Sexp.to_string))

module Wrap = struct
  include T

  let assign ?loc var_id expr =
    let var_id = Var.Ir.unwrap var_id in
    let expr = Expr.Ir.unwrap expr in
    fail_if_layout_does_not_fit_dtype
      "Assignment of expression with max_layout %s into variable with layout \
       %s. It might not fit. Try using a custom assignment statment (e.g. \
       CInt.N.assign)."
      (Expr.Ir.max_layout expr) var_id.u.d.dtype;
    No_width_checks.assign ?loc var_id expr

  let read ?loc chan_id var_id =
    let chan_id = Chan.Ir.unwrap_r chan_id in
    let var_id = Var.Ir.unwrap var_id in
    fail_if_layout_does_not_fit_dtype
      "Read of channel with layout %s into variable with layout %s. It might \
       not fit in. This is currently unsupported, so you must use a variable \
       with a larger layout."
      (Dtype.Ir.layout chan_id.d.dtype)
      var_id.u.d.dtype;
    No_width_checks.read ?loc chan_id var_id

  let send ?loc chan_id expr =
    let chan_id = Chan.Ir.unwrap_w chan_id in
    let expr = Expr.Ir.unwrap expr in
    fail_if_layout_does_not_fit_dtype
      "Send of expression with layout %s into channel with layout %s. It might \
       not fit in. Try using a custom send statment (e.g. CInt.N.send)."
      (Expr.Ir.max_layout expr) chan_id.d.dtype;
    No_width_checks.send ?loc chan_id expr

  let send' ?loc chan_id var_id = send ?loc chan_id Expr.Wrap.(var var_id)

  let wait_probe_r ?loc chan_id =
    let loc = Code_pos.value_or_psite loc in
    let chan = Chan.Ir.unwrap_r chan_id in
    (match chan.d.wait_sendable_code_pos with
    | Some wait_sendable_code_pos ->
        failwith
          [%string
            "Trying to wait until a channel is readable, but line \
             %{wait_sendable_code_pos.line_number#Int} in \
             %{wait_sendable_code_pos.filename} is waiting for this channel to \
             be sendable. A channel can be probed on at most one side."]
    | None ->
        if Option.is_none chan.d.wait_readable_code_pos then
          chan.d.wait_readable_code_pos <- Some loc);
    WaitUntilSendReady (loc, chan)

  let wait_probe_w ?loc chan_id =
    let loc = Code_pos.value_or_psite loc in
    let chan = Chan.Ir.unwrap_w chan_id in
    (match chan.d.wait_readable_code_pos with
    | Some wait_readable_code_pos ->
        failwith
          [%string
            "Trying to wait until a channel is sendable, but line \
             %{wait_readable_code_pos.line_number#Int} in \
             %{wait_readable_code_pos.filename} is waiting for this channel to \
             be readable. A channel can be probed on at most one side."]
    | None ->
        if Option.is_none chan.d.wait_sendable_code_pos then
          chan.d.wait_sendable_code_pos <- Some loc);
    WaitUntilReadReady (loc, chan)

  let select_probe_r ?loc:_ _ = failwith "TODO"
  let select_probe_w ?loc:_ _ = failwith "TODO"
  let select_probe ?loc:_ _ = failwith "TODO"

  (* interacting with memories *)
  let read_ug_mem ?loc (mem : 'a Mem.Wrap.ug_mem) ~idx ~(dst : 'a Var.Wrap.t) =
    let mem = Mem.Ir.unwrap_ug_mem mem in
    let dst = Var.Ir.unwrap dst in
    fail_if_layout_does_not_fit_dtype
      "Read of memory with cell layout %s into variable with layout %s. It \
       might not fit in. This is currently unsupported, so you must use a ~dst \
       variable with a larger layout."
      (Dtype.Ir.layout mem.d.dtype)
      dst.u.d.dtype;
    ReadUGMem
      (Code_pos.value_or_psite loc, mem, Expr.Ir.unwrap idx, Var.Ir.untype dst)

  (* TODO treat idx like other expressions? *)
  let write_ug_mem ?loc (mem : 'a Mem.Wrap.ug_mem) ~idx
      ~(value : 'a Expr.Wrap.t) =
    let mem = Mem.Ir.unwrap_ug_mem mem in
    let value = Expr.Ir.unwrap value in
    fail_if_layout_does_not_fit_dtype
      "Write of expression with layout %s into a memory with cell layout %s. \
       It might not fit in. Try using a custom write_ug_mem statment (e.g. \
       CInt.N.write_ug_mem)."
      (Expr.Ir.max_layout value) mem.d.dtype;
    WriteUGMem
      ( Code_pos.value_or_psite loc,
        mem,
        Expr.Ir.unwrap idx,
        Expr.Ir.untype value )

  let write_ug_mem' ?loc (mem : 'a Mem.Wrap.ug_mem) ~idx
      ~(value : 'a Var.Wrap.t) =
    write_ug_mem ?loc mem ~idx ~value:Expr.Wrap.(var value)

  let read_ug_rom ?loc (rom : 'a Mem.Wrap.ug_rom) ~idx ~(dst : 'a Var.Wrap.t) =
    let rom = Mem.Ir.unwrap_ug_rom rom in
    let dst = Var.Ir.unwrap dst in
    fail_if_layout_does_not_fit_dtype
      "Read of rom with cell layout %s into variable with layout %s. It might \
       not fit in. This is currently unsupported, so you must use a ~dst \
       variable with a larger layout."
      (Dtype.Ir.layout rom.d.dtype)
      dst.u.d.dtype;
    ReadUGMem
      (Code_pos.value_or_psite loc, rom, Expr.Ir.unwrap idx, Var.Ir.untype dst)

  let log ?loc str = Log (Code_pos.value_or_psite loc, str)

  let log1' ?loc expr ~f =
    Log1 (Code_pos.value_or_psite loc, Expr.Ir.untype' expr, Obj.magic f)

  let log1 ?loc var ~f = log1' ?loc (Expr.Wrap.var var) ~f

  let assert_ ?loc expr =
    Assert (Code_pos.value_or_psite loc, Expr.Ir.unwrap expr)

  let seq ?loc l = Seq (Code_pos.value_or_psite loc, l)
  let par ?loc l = Par (Code_pos.value_or_psite loc, l)

  let if_else ?loc expr t_br f_br =
    SelectImm
      ( Code_pos.value_or_psite loc,
        [ (Expr.Ir.unwrap expr, seq ?loc t_br) ],
        Some (seq ?loc f_br) )

  let loop ?loc t = Loop (Code_pos.value_or_psite loc, seq ?loc t)

  let while_loop ?loc expr t =
    WhileLoop (Code_pos.value_or_psite loc, Expr.Ir.unwrap expr, seq ?loc t)

  let select_imm ?loc branches ~else_ =
    SelectImm
      ( Code_pos.value_or_psite loc,
        List.map branches ~f:(fun (guard, stmt) -> (Expr.Ir.unwrap guard, stmt)),
        else_ )

  module Overflow_behavior = struct
    type t = Cant | Mask [@@deriving sexp]
  end
end
