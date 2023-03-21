open! Core

type t = Ir_chp.t [@@deriving sexp_of]

module Internal = struct
  let unwrap t = t
  let wrap t = t
end

let fail_if_layout_does_not_fit_dtype err_msg layout dtype =
  match Ir_dtype.fits_into_dtype layout ~into:dtype with
  | true -> ()
  | false ->
      failwith
        (sprintf err_msg
           (Ir_layout.sexp_of_t layout |> Sexp.to_string)
           (Ir_dtype.layout dtype |> Ir_layout.sexp_of_t |> Sexp.to_string))

let assign var_id expr =
  let loc = Code_pos.psite () in
  let var_id = Var.Internal.unwrap var_id in
  let expr = Expr.Internal.unwrap expr in
  fail_if_layout_does_not_fit_dtype
    "Assignment of expression with max_layout %s into variable with layout %s. \
     It might not fit. Try using a custom assignment statment (e.g. \
     Cint.N.assign)."
    (Ir_expr.max_layout expr) var_id.u.d.dtype;
  Ir_chp.Assign (loc, var_id.u, Ir_expr.untype expr)

let read chan_id var_id =
  let loc = Code_pos.psite () in
  let chan_id = Chan.Internal.unwrap_r chan_id in
  let var_id = Var.Internal.unwrap var_id in
  fail_if_layout_does_not_fit_dtype
    "Read of channel with layout %s into variable with layout %s. It might not \
     fit in. This is currently unsupported, so you must use a variable with a \
     larger layout."
    (Ir_dtype.layout chan_id.d.dtype)
    var_id.u.d.dtype;
  Ir_chp.Read (loc, chan_id, var_id.u)

let send chan_id expr =
  let loc = Code_pos.psite () in
  let chan_id = Chan.Internal.unwrap_w chan_id in
  let expr = Expr.Internal.unwrap expr in
  fail_if_layout_does_not_fit_dtype
    "Send of expression with layout %s into channel with layout %s. It might \
     not fit in. Try using a custom send statment (e.g. Cint.N.send)."
    (Ir_expr.max_layout expr) chan_id.d.dtype;
  Ir_chp.Send (loc, chan_id, Ir_expr.untype expr)

let send_var chan_id var_id = send chan_id Expr.(var var_id)

let wait_probe_r chan_id =
  let loc = Code_pos.psite () in
  let chan = Chan.Internal.unwrap_r chan_id in
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
  Ir_chp.WaitUntilSendReady (loc, chan)

let wait_probe_w chan_id =
  let loc = Code_pos.psite () in
  let chan = Chan.Internal.unwrap_w chan_id in
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
  Ir_chp.WaitUntilReadReady (loc, chan)

let select_probe_r _ = failwith "TODO"
let select_probe_w _ = failwith "TODO"
let select_probe _ = failwith "TODO"

(* interacting with memories *)
let read_ug_mem (mem : 'a Mem.ug_mem) ~idx ~(dst : 'a Var.t) =
  let mem = Mem.Internal.unwrap_ug_mem mem in
  let dst = Var.Internal.unwrap dst in
  fail_if_layout_does_not_fit_dtype
    "Read of memory with cell layout %s into variable with layout %s. It might \
     not fit in. This is currently unsupported, so you must use a ~dst \
     variable with a larger layout."
    (Ir_dtype.layout mem.d.dtype)
    dst.u.d.dtype;
  Ir_chp.ReadUGMem (Code_pos.psite (), mem, Expr.Internal.unwrap idx, dst.u)

(* TODO treat idx like other expressions? *)
let write_ug_mem (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Expr.t) =
  let mem = Mem.Internal.unwrap_ug_mem mem in
  let value = Expr.Internal.unwrap value in
  fail_if_layout_does_not_fit_dtype
    "Write of expression with layout %s into a memory with cell layout %s. It \
     might not fit in. Try using a custom write_ug_mem statment (e.g. \
     Cint.N.write_ug_mem)."
    (Ir_expr.max_layout value) mem.d.dtype;
  Ir_chp.WriteUGMem
    (Code_pos.psite (), mem, Expr.Internal.unwrap idx, Ir_expr.untype value)

let write_ug_mem' (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Var.t) =
  write_ug_mem mem ~idx ~value:Expr.(var value)

let read_ug_rom (rom : 'a Mem.ug_rom) ~idx ~(dst : 'a Var.t) =
  let rom = Mem.Internal.unwrap_ug_rom rom in
  let dst = Var.Internal.unwrap dst in
  fail_if_layout_does_not_fit_dtype
    "Read of rom with cell layout %s into variable with layout %s. It might \
     not fit in. This is currently unsupported, so you must use a ~dst \
     variable with a larger layout."
    (Ir_dtype.layout rom.d.dtype)
    dst.u.d.dtype;
  Ir_chp.ReadUGMem (Code_pos.psite (), rom, Expr.Internal.unwrap idx, dst.u)

let log str = Ir_chp.Log (Code_pos.psite (), str)

let log1' expr ~f =
  let expr = Expr.Internal.unwrap expr in
  Ir_chp.Log1 (Code_pos.psite (), Ir_expr.untype expr, Obj.magic f)

let log1 var ~f = log1' (Expr.var var) ~f
let assert_ expr = Ir_chp.Assert (Code_pos.psite (), Expr.Internal.unwrap expr)
let seq l = Ir_chp.Seq (Code_pos.psite (), l)
let par l = Ir_chp.Par (Code_pos.psite (), l)

let if_else expr t_br f_br =
  Ir_chp.SelectImm
    ( Code_pos.psite (),
      [ (Expr.Internal.unwrap expr, seq t_br) ],
      Some (seq f_br) )

let loop t = Ir_chp.Loop (Code_pos.psite (), seq t)

let while_loop expr t =
  Ir_chp.WhileLoop (Code_pos.psite (), Expr.Internal.unwrap expr, seq t)

let select_imm branches ~else_ =
  Ir_chp.SelectImm
    ( Code_pos.psite (),
      List.map branches ~f:(fun (guard, stmt) ->
          (Expr.Internal.unwrap guard, stmt)),
      else_ )
