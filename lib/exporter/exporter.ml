open! Core

let export_program program ~user_sendable_ports ~user_readable_ports =
  let flat_program =
    Flat_program.of_program program ~user_sendable_ports ~user_readable_ports
  in
  let s = Opt_program.export_program flat_program in
  printf "%s" s

let export_chp chp ~user_sendable_ports ~user_readable_ports =
  let program = Act.Program.of_procs [ Act.Process.of_chp chp ] in
  export_program program ~user_sendable_ports ~user_readable_ports
