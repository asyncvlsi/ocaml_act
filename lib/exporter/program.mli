open! Core

module Mem_proc : sig
  type t = {
    init : Act.CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Interproc_chan.t;
    read_chan : Interproc_chan.t;
    write_chan : Interproc_chan.t option;
  }
  [@@deriving sexp_of]
end

module Process : sig
  module K : sig
    type t = Chp of Flat_chp.Proc.t | Mem of Mem_proc.t [@@deriving sexp_of]
  end

  type t = { k : K.t }
end

type t = {
  processes : Process.t list;
  top_iports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
  top_oports : (Interproc_chan.t * Act.Internal_rep.Chan.U.t) list;
}
[@@deriving sexp_of]

val of_program :
  Act.Program.t ->
  user_sendable_ports:Act.Chan.W.U.t list ->
  user_readable_ports:Act.Chan.R.U.t list ->
  t
