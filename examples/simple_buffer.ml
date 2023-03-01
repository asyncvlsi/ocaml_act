open! Core
open! Act

(* $MDX part-begin=simple_buffer_example *)
(* This function generated the IR for a simple buffer. It then returns the IR,
   along with the "write end" of the input channel and the "read end" of the
   output channel. *)
let simple_buffer () =
  let i = Chan.create CInt.dtype_32 in
  let x = Var.create CInt.dtype_32 in
  let o = Chan.create CInt.dtype_32 in
  let ir = Chp.loop [ Chp.read i.r x; Chp.send_var o.w x ] in
  (ir, i.w, o.r)

let%expect_test "test" =
  (* Instantiate the buffer *)
  let ir, i, o = simple_buffer () in
  (* create a simulation *)
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in

  (* Set up a simulation step. We will send the value `3` on `i`, and check that
     we can read a `3` on `o`. *)
  Sim.send sim i CInt.three;
  Sim.read sim o CInt.three;

  (* Then, wait for all the queued action to complete. If anything goes wrong,
     or if any actions dont compelte, print an error *)
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];

  (* This contains the expected contences of stdout after running the test. See
     the section on expect_tests in
     https://dev.realworldocaml.org/testing.html. *)
  [%expect {| (Ok ()) |}]
(* $MDX part-end *)
