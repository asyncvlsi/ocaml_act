open! Core

type t = Act_ir.Ir.Chp.t [@@deriving sexp_of]

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
           (Layout.sexp_of_t layout |> Sexp.to_string)
           (Ir_dtype.layout dtype |> Layout.sexp_of_t |> Sexp.to_string))

let assign var_id expr =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let var_dtype = Var.Internal.dtype var_id in
  fail_if_layout_does_not_fit_dtype
    "Assignment of expression with max_layout %s into variable with layout %s. \
     It might not fit. Try using a custom assignment statment (e.g. \
     Cint.N.assign)."
    (Bits_fixed (Expr.Internal.max_bits expr))
    var_dtype;
  let var_id = Var.Internal.unwrap var_id in
  let expr = Expr.Internal.unwrap expr in
  let m = Act_ir.Ir.Chp.M.create ~var_sexper:var_dtype.sexp_of_cint loc in
  Act_ir.Ir.Chp.assign ~m var_id expr

let read chan_id var_id =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let chan_dtype = Chan.Internal.dtype_r chan_id in
  let chan_id = Chan.Internal.unwrap_r chan_id in
  let var_dtype = Var.Internal.dtype var_id in
  let var_id = Var.Internal.unwrap var_id in
  fail_if_layout_does_not_fit_dtype
    "Read of channel with layout %s into variable with layout %s. It might not \
     fit in. This is currently unsupported, so you must use a variable with a \
     larger layout."
    (Ir_dtype.layout chan_dtype)
    var_dtype;
  let m = Act_ir.Ir.Chp.M.create ~var_sexper:var_dtype.sexp_of_cint loc in
  Act_ir.Ir.Chp.read ~m chan_id var_id

let send chan_id expr =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let chan_dtype = Chan.Internal.dtype_w chan_id in
  (* print_s [%sexp (chan_id: _ Chan.W.t)]; *)
  fail_if_layout_does_not_fit_dtype
    "Send of expression with layout %s into channel with layout %s. It might \
     not fit in. Try using a custom send statment (e.g. Cint.N.send)."
    (Bits_fixed (Expr.Internal.max_bits expr))
    chan_dtype;
  let expr = Expr.Internal.unwrap expr in
  let chan_id = Chan.Internal.unwrap_w chan_id in
  let m = Act_ir.Ir.Chp.M.create ~chan_sexper:chan_dtype.sexp_of_cint loc in
  Act_ir.Ir.Chp.send ~m chan_id expr

let send_var chan_id var_id = send chan_id Expr.(var var_id)

let wait_probe_r chan_id =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let chan = Chan.Internal.unwrap_r_inner chan_id in
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
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.nondeterm_select ~m [ (Send chan.c, Act_ir.Ir.Chp.nop ()) ]

let wait_probe_w chan_id =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let chan = Chan.Internal.unwrap_w_inner chan_id in
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
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.nondeterm_select ~m [ (Read chan.c, Act_ir.Ir.Chp.nop ()) ]

let select_probe_r _ = failwith "TODO select_probe_r"
let select_probe_w _ = failwith "TODO select_probe_w"
let select_probe _ = failwith "TODO select_probe"

(* interacting with memories *)
let read_ug_mem (mem : 'a Mem.ug_mem) ~idx ~(dst : 'a Var.t) =
  let mem_dtype = Mem.Internal.dtype mem in
  let mem = Mem.Internal.unwrap_ug_mem mem in
  let dst_dtype = Var.Internal.dtype dst in
  let dst = Var.Internal.unwrap dst in
  fail_if_layout_does_not_fit_dtype
    "Read of memory with cell layout %s into variable with layout %s. It might \
     not fit in. This is currently unsupported, so you must use a ~dst \
     variable with a larger layout."
    (Ir_dtype.layout mem_dtype)
    dst_dtype;
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc ~var_sexper:dst_dtype.sexp_of_cint in
  Act_ir.Ir.Chp.read_mem ~m mem ~idx:(Expr.Internal.unwrap idx) ~dst

(* TODO treat idx like other expressions? *)
let write_ug_mem (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Expr.t) =
  let mem_dtype = Mem.Internal.dtype mem in
  let mem = Mem.Internal.unwrap_ug_mem mem in
  fail_if_layout_does_not_fit_dtype
    "Write of expression with layout %s into a memory with cell layout %s. It \
     might not fit in. Try using a custom write_ug_mem statment (e.g. \
     Cint.N.write_ug_mem)."
    (Bits_fixed (Expr.Internal.max_bits value))
    mem_dtype;
  let value = Expr.Internal.unwrap value in
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc ~cell_sexper:mem_dtype.sexp_of_cint in
  Act_ir.Ir.Chp.write_mem ~m mem ~idx:(Expr.Internal.unwrap idx) ~value

let write_ug_mem' (mem : 'a Mem.ug_mem) ~idx ~(value : 'a Var.t) =
  write_ug_mem mem ~idx ~value:Expr.(var value)

let read_ug_rom (rom : 'a Mem.ug_rom) ~idx ~(dst : 'a Var.t) =
  let rom_dtype = Mem.Internal.dtype rom in
  let rom = Mem.Internal.unwrap_ug_rom rom in
  let dst_dtype = Var.Internal.dtype dst in
  let dst = Var.Internal.unwrap dst in
  fail_if_layout_does_not_fit_dtype
    "Read of rom with cell layout %s into variable with layout %s. It might \
     not fit in. This is currently unsupported, so you must use a ~dst \
     variable with a larger layout."
    (Ir_dtype.layout rom_dtype)
    dst_dtype;
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc ~var_sexper:dst_dtype.sexp_of_cint in
  Act_ir.Ir.Chp.read_mem ~m rom ~idx:(Expr.Internal.unwrap idx) ~dst

let log str =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.log ~m str

let log1' (expr : 'a Expr.t) ~(f : 'a -> string) =
  let f v =
    Expr_tag.value_of_cint (Expr.Internal.tag expr) v
    |> Option.map ~f |> Option.value ~default:""
  in
  let expr = Expr.Internal.unwrap expr in
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.log1 ~m expr ~f

let log1 var ~f = log1' (Expr.var var) ~f

let assert_ expr =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.assert_ ~m (Expr.Internal.unwrap expr)

let seq l =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.seq ~m l

let par l =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.par ~m l

let if_else expr t_br f_br =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.if_else ~m (Expr.Internal.unwrap expr) t_br f_br

let loop t =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.loop ~m t

let while_loop expr t =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  Act_ir.Ir.Chp.while_loop ~m (Expr.Internal.unwrap expr) t

let select_imm branches ~else_ =
  let loc = Act_ir.Utils.Code_pos.psite () in
  let m = Act_ir.Ir.Chp.M.create loc in
  let branches =
    List.map branches ~f:(fun (guard, stmt) ->
        (Expr.Internal.unwrap guard, stmt))
  in
  Act_ir.Ir.Chp.select_imm ~m branches ~else_
