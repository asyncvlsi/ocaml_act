open! Core
open! Act

let%expect_test "test probes" =
  let var = Var.create CInt.dtype_32 in
  let chan = Chan.create CInt.dtype_32 in
  let ir =
    Chp.par
      [
        Chp.seq
          [
            Chp.log "A ";
            Chp.wait_probe_w chan.w;
            Chp.log "B ";
            Chp.log "C ";
            Chp.send chan.w CInt.E.(of_int 3);
            Chp.log "D ";
            Chp.log "E ";
            Chp.log "F ";
          ];
        Chp.seq
          [
            Chp.log "1 ";
            Chp.log "2 ";
            Chp.log "3 ";
            Chp.log "4 ";
            Chp.read chan.r var;
            Chp.log "5 ";
          ];
      ]
  in
  let sim =
    Sim.simulate_chp ir ~user_sendable_ports:[ chan.w.u ]
      ~user_readable_ports:[] ~seed:777
  in
  Sim.wait' sim ();
  [%expect {|
    A 1 2 3 4 B C D E 5 F (Ok ()) |}]
