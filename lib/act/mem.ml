open! Core

type ('kind, 'a) t = Ir_mem.t [@@deriving sexp_of]
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
               %{Ir_layout.sexp_of_t (Ir_dtype.layout dtype)#Sexp} with a \
               value of max_layout %{Ir_layout.sexp_of_t init_layout#Sexp}."]);
  Ir_mem.create dtype creation_code_pos init kind

let create_ug_mem dtype (arr : 'a array) : 'a ug_mem =
  let loc = Code_pos.psite () in
  create dtype arr loc `Mem

let create_ug_rom dtype (arr : 'a array) : 'a ug_rom =
  let loc = Code_pos.psite () in
  create dtype arr loc `Rom

module Internal = struct
  let unwrap_ug_mem t = t
  let unwrap_ug_rom t = t
end
