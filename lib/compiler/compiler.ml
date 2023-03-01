open! Core

let compile process ~to_ =
  let program = Act.Internal_rep.Process.unwrap process |> Program.of_process in
  match to_ with
  | `Chp_and_dataflow -> Opt_program.export_program program
  | `Prod_rules -> failwith "TODO"

let compile_chp chp ~user_sendable_ports ~user_readable_ports ~to_ =
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
  compile process ~to_
