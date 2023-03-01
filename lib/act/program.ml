open! Core
(* module Ir = struct type t = Process.Ir.t list [@@deriving sexp_of] type outer
   = t

   let unwrap t = t let of_procs l = (* TODO do checks *) List.map l
   ~f:Process.Ir.unwrap end

   include Ir *)
