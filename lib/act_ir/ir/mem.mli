open! Core
open Utils
module Id : Identifiable

type t = {
  id : Id.t;
  cell_bitwidth : int;
  creation_code_pos : Utils.Code_pos.t;
  init : (CInt.t array[@hash.ignore] [@compare.ignore] [@equal.ignore]);
  kind : [ `Mem | `Rom ];
}
[@@deriving sexp_of]

include Comparable with type t := t
include Hashable with type t := t

val create : int -> Utils.Code_pos.t -> CInt.t array -> [ `Mem | `Rom ] -> t
