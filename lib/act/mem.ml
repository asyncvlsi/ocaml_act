open! Core

type ('kind, 'a) t = { dtype : 'a Ir_dtype.t; m : Act_ir.Mem.t }
[@@deriving sexp_of]

type 'a ug_mem = ([ `Mem ], 'a) t
type 'a ug_rom = ([ `Rom ], 'a) t

let create dtype init creation_code_pos kind =
  let dtype = Dtype.Internal.unwrap dtype in
  let init = Array.copy init in
  Array.iteri init ~f:(fun i init ->
      let init_layout = Ir_dtype.max_layout_of dtype init in
      match Ir_dtype.fits_into_dtype init_layout ~into:dtype with
      | true -> ()
      | false ->
          failwith
            [%string
              "Trying to initialize cell %{i#Int} of memory of dtype \
               %{Layout.sexp_of_t (Ir_dtype.layout dtype)#Sexp} with a \
               value of max_layout %{Layout.sexp_of_t \
               init_layout#Sexp}."]);
  let init = Array.map init ~f:dtype.Ir_dtype.cint_of in
  let cell_bitwidth =
    match dtype.layout with Bits_fixed bitwidth -> bitwidth
  in
  { dtype; m = Act_ir.Mem.create cell_bitwidth creation_code_pos init kind }

let create_ug_mem dtype (arr : 'a array) : 'a ug_mem =
  let loc = Act_ir.Utils.Code_pos.psite () in
  create dtype arr loc `Mem

let create_ug_rom dtype (arr : 'a array) : 'a ug_rom =
  let loc = Act_ir.Utils.Code_pos.psite () in
  create dtype arr loc `Rom

module Internal = struct
  let unwrap_ug_mem t = t.m
  let unwrap_ug_rom t = t.m
  let dtype t = t.dtype
end
