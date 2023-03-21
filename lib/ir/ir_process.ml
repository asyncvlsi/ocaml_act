open! Core

type inner =
  | Chp of Ir_chp.t
  | Dflow_iface_on_chp of Ir_chp.t
  | Subprocs of t list

and t = { inner : inner; iports : Ir_chan.U.Set.t; oports : Ir_chan.U.Set.t }
[@@deriving sexp_of]

module Inner = struct
  type nonrec t = inner =
    | Chp of Ir_chp.t
    | Dflow_iface_on_chp of Ir_chp.t
    | Subprocs of t list
  [@@deriving sexp_of]
end
