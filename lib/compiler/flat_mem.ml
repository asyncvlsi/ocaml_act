open! Core
module CInt = Act.CInt

module Proc = struct
  type t = {
    init : CInt.t array;
    idx_bits : int;
    cell_bits : int;
    cmd_chan : Interproc_chan.t;
    read_chan : Interproc_chan.t;
    write_chan : Interproc_chan.t option;
  }
  [@@deriving sexp_of]
end
