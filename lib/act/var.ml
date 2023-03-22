open! Core

type 'a t = { dtype : 'a Ir_dtype.t; v : Ir_var.t } [@@deriving sexp_of]

let create ?init (dtype : 'a Dtype.t) : 'a t =
  let loc = Code_pos.psite () in
  let dtype = Dtype.Internal.unwrap dtype in
  (match init with
  | None -> ()
  | Some init -> (
      let init_layout = Ir_dtype.max_layout_of dtype init in
      match Ir_dtype.fits_into_dtype init_layout ~into:dtype with
      | true -> ()
      | false ->
          failwith
            [%string
              "Trying to initialize a variable of dtype %{Ir_layout.sexp_of_t \
               (Ir_dtype.layout dtype)#Sexp} with a value of max_layout \
               %{Ir_layout.sexp_of_t init_layout#Sexp}."]));
  let init = Option.map init ~f:dtype.Ir_dtype.cint_of in
  let bitwidth = match dtype.layout with Bits_fixed bitwidth -> bitwidth in
  let v = Ir_var.create bitwidth loc init in
  { dtype; v }

module Internal = struct
  let unwrap t = t.v
  let dtype t = t.dtype
end
