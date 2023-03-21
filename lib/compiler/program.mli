open! Core

module Process : sig
  module K : sig
    type t = Chp of Flat_chp.Proc.t | Mem of Flat_mem.Proc.t
    [@@deriving sexp_of]
  end

  type t = { k : K.t }
end

type t = {
  processes : Process.t list;
  top_iports : (Interproc_chan.t * Ir_chan.U.t) list;
  top_oports : (Interproc_chan.t * Ir_chan.U.t) list;
}
[@@deriving sexp_of]

val of_process : Ir_process.t -> t
