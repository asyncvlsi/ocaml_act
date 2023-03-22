open! Core

type inner = Chp of Chp.t | Dflow_iface_on_chp of Chp.t | Subprocs of t list

and t = { inner : inner; iports : Chan.Set.t; oports : Chan.Set.t }
[@@deriving sexp_of]
(** @canonical Act_ir.Ir.Process.t *)

module Inner : sig
  type nonrec t = inner =
    | Chp of Chp.t
    | Dflow_iface_on_chp of Chp.t
    | Subprocs of t list
  [@@deriving sexp_of]
end
