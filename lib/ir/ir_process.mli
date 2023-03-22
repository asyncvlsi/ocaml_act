open! Core

type inner =
  | Chp of Ir_chp.t
  | Dflow_iface_on_chp of Ir_chp.t
  | Subprocs of t list

and t = { inner : inner; iports : Ir_chan.Set.t; oports : Ir_chan.Set.t }
[@@deriving sexp_of]

module Inner : sig
  type nonrec t = inner =
    | Chp of Ir_chp.t
    | Dflow_iface_on_chp of Ir_chp.t
    | Subprocs of t list
  [@@deriving sexp_of]
end