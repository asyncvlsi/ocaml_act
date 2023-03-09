open! Core

module Compiled_program = struct
  type t = Opt_program.t [@@deriving sexp_of]

  let of_process process ~to_ =
    let program =
      Act.Internal_rep.Process.unwrap process |> Program.of_process
    in
    match to_ with
    | `Chp_and_dataflow -> Opt_program.of_prog program
    | `Prod_rules -> failwith "TODO"

  let of_chp_process chp ~user_sendable_ports ~user_readable_ports ~to_ =
    let iports =
      user_sendable_ports |> List.map ~f:Act.Internal_rep.Chan.ru_of_wu
    in
    let oports =
      user_readable_ports |> List.map ~f:Act.Internal_rep.Chan.wu_of_ru
    in
    let process =
      match to_ with
      | `Chp -> Act.Process.of_chp chp ~iports ~oports
      | `Dataflow ->
          Act.Process.of_chp ~with_dflow_interface:true chp ~iports ~oports
      | `Prod_rules -> failwith "TODO"
    in
    let to_ = `Chp_and_dataflow in
    of_process process ~to_
end

let compile process ~to_ = Compiled_program.of_process process ~to_

let compile_chp chp ~user_sendable_ports ~user_readable_ports ~to_ =
  Compiled_program.of_chp_process chp ~user_sendable_ports ~user_readable_ports
    ~to_

let export t = Opt_program.export t

let export_print t =
  let s = export t in
  printf "%s" s
