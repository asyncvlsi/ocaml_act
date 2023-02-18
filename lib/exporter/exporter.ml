open! Core

let export_program program ~user_sendable_ports ~user_readable_ports =
  let flat_program =
    Program.of_program program ~user_sendable_ports ~user_readable_ports
  in
  let s = Opt_program.export_program flat_program in
  printf "%s" s

let export_chp ?(as_dflow = false) chp ~user_sendable_ports ~user_readable_ports
    =
  let program =
    match as_dflow with
    | true -> Act.Program.of_procs [ Act.Process.dflow_iface_on_chp chp ]
    | false -> Act.Program.of_procs [ Act.Process.of_chp chp ]
  in

  export_program program ~user_sendable_ports ~user_readable_ports
