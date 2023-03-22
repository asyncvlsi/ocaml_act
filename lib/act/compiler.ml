open! Core
module Compiled_program = Act_ir.Compiler.Compiled_program

let compile process ~to_ =
  Act_ir.Compiler.compile (Process.Internal.unwrap process) ~to_

let compile_chp chp ~user_sendable_ports ~user_readable_ports ~to_ =
  let iports = List.map user_sendable_ports ~f:Chan.Internal.ru_of_wu in
  let oports = List.map user_readable_ports ~f:Chan.Internal.wu_of_ru in
  let process, to_ =
    match to_ with
    | `Chp -> (Process.of_chp chp ~iports ~oports, `Chp_and_dataflow)
    | `Dataflow ->
        ( Process.of_chp ~with_dflow_interface:true chp ~iports ~oports,
          `Chp_and_dataflow )
    | `Prod_rules -> (failwith "Not yet supported", `Prod_rules)
  in
  compile process ~to_

let export t = Act_ir.Compiler.export t
let export_print t = Act_ir.Compiler.export_print t
let sim ?seed t = Act_ir.Compiler.sim ?seed t |> Sim.Internal.wrap
