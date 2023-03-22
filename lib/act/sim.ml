open! Core

type t = Act_ir.Sim.t [@@deriving sexp_of]

let simulate ?seed process =
  Act_ir.Sim.simulate ?seed (Process.Internal.unwrap process)

let simulate_chp ?seed chp ~user_sendable_ports ~user_readable_ports =
  let iports = List.map user_sendable_ports ~f:Chan.Internal.ru_of_wu in
  let oports = List.map user_readable_ports ~f:Chan.Internal.wu_of_ru in
  let process = Process.of_chp chp ~iports ~oports in
  simulate ?seed process

let wait t ?max_steps ?line_numbers () =
  Act_ir.Sim.wait t ?max_steps ?line_numbers ()

let wait' t ?max_steps () = Act_ir.Sim.wait' t ?max_steps ()

let send t chan value =
  let chan = Chan.Internal.unwrap_w_inner chan in
  let value = chan.d.dtype.cint_of (Act_ir.Utils.Any.of_magic value) in
  Act_ir.Sim.send t chan.c value

let read t chan value =
  let chan = Chan.Internal.unwrap_r_inner chan in
  let value = chan.d.dtype.cint_of (Act_ir.Utils.Any.of_magic value) in
  Act_ir.Sim.read t chan.c value

module Internal = struct
  let wrap t = t
end
