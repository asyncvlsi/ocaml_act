open! Core

module Process : sig
  type t =
    | Chp of Flat_chp.Proc.t
    | Dflow of Flat_dflow.Proc.t
    | Mem of Flat_mem.Proc.t
  [@@deriving sexp_of]
end

type t = {
  processes : Process.t list;
  top_iports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
  top_oports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
}
[@@deriving sexp_of]

val of_prog : Program.t -> t
val export : t -> string
