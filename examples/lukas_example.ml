(* $MDX part-begin=lukas_example *)
(* First, importing the Jane Street's Core library and the OCaml Act library *)
open! Core
open! Act

(* Second, create a function. This will be the equivilent of the chp process.
   There is no distinction between what would be a templated variable and a in
   chp variable in the act langauge. This is because, in the OCaml library, the
   syntax tree is all built at runtime. The tilde before the arguments make
   these "named arguments". This makes it difficult to flip the order of
   arguments to the function. *)
let kmac_cjp ~bw ~kernel ~a ~leftinput ~command ~rightout ~out =
  (* Note that as currently designed, there is no check on the input channel
     bitwidths automatically, and so you need to check it manually. Note that
     the bitwidth interface is still under development and may be subject to
     change. *)
  assert (CInt.Chan.bw_le kernel bw);
  assert (CInt.Chan.bw_le a bw);
  assert (CInt.Chan.bw_le leftinput bw);
  assert (CInt.Chan.bw_le command bw);
  assert (CInt.Chan.bw_ge rightout bw);
  assert (CInt.Chan.bw_ge out bw);
  (* Now create the variables used within the process. Note that we initialize
     outputx to be zero by providing a optional value for the `init` argument in
     Var.create. *)
  let val_dtype = CInt.dtype ~bits:bw in
  let inputx = Var.create val_dtype in
  let tininputx = Var.create val_dtype in
  let toutinputx = Var.create val_dtype in
  let outputx = Var.create val_dtype ~init:CInt.zero in
  let kernelV = Var.create val_dtype in
  let c = Var.create val_dtype in
  (* Now we write some CHP. We start by writing the wrapper loop.

     The `Chp.loop` function takes in a list of Chp nodes and returns a node
     representing an infinite loop whose body is those nodes in sequence. *)
  Chp.loop
    [
      (* The `Chp.read` function takes in a `Chan.R.t` (the read end of a chan)
         and a `Var.t`. It returns a node rerpesenting reading that channel into
         that variable. *)
      Chp.read command c;
      (* 

         Now we will add a select statement, using the function `Chp.select_imm
         <statements> ~else_:<statement>`. This is a function take in a list of
         tuples of the type `(<expression guard>, <CHP statement>`). It
         represents a deterministic select statement in chp with no probes as
         guards. It is our job (as chp writers) to guardentee that at most one
         guard is true when this statement is reached. If ther is no else branch
         provided, we must further guarentee that exactly one guard is true when
         this node is reached. Because it is easy to forget an else branch when
         what one really wants is a "trivial" else branch, the else argument is
         not option. The arguement has name `else_` instead of `else` because
         `else` is a reserved keyword by OCaml. For the following, we'll use
         None for the else, meaning that we guarentee that the value of `c` will
         always be one of [0;1;5;6;7;8;9].

         The syntax `Expr.(<statements> )` is a "local module open" (See section
         "Prefer Local Opens" in
         https://dev.realworldocaml.org/files-modules-and-programs.html). This
         is simply a way to compress repeating the module name. In particular
         `Expr.(eq (var c) (one))` is exactly equivilent to `(Expr.eq (Expr.var
         c) (Expr.one))`. But, as you can see, it requires many fewer
         characters.

         Go look at lib/act/expr.mli and familiarize yourself with the available
         functions. *)
      Chp.select_imm
        [
          ( Expr.(eq (var c) zero),
            CInt.Chp.assign ~overflow:Mask outputx
              Expr.(add (mul (var inputx) (var kernelV)) (var outputx)) );
          ( Expr.(eq (var c) one),
            Chp.seq
              [
                Chp.assign toutinputx CInt.E.(var tininputx);
                Chp.par
                  [
                    Chp.send_var rightout toutinputx;
                    Chp.read leftinput tininputx;
                  ];
              ] );
          ( Expr.(eq (var c) (of_int 5)),
            Chp.assign tininputx CInt.E.(var inputx) );
          (Expr.(eq (var c) (of_int 6)), Chp.send_var out outputx);
          (Expr.(eq (var c) (of_int 7)), Chp.read kernel kernelV);
          (Expr.(eq (var c) (of_int 8)), Chp.read a inputx);
          (Expr.(eq (var c) (of_int 9)), Chp.assign outputx Expr.zero);
        ]
        ~else_:None;
    ]

(* Great! This is now a complete implementation of the chp process above. Now,
   to check our implementation, we should write some tests!

   We will use the OCaml expect_test library (See
   https://dev.realworldocaml.org/testing.html). *)

let%expect_test "kmac_cjp_test_1" =
  (* create all the channels that the processes uses as io *)
  let bw = 8 in
  let kernel = Chan.create (CInt.dtype ~bits:bw) in
  let a = Chan.create (CInt.dtype ~bits:bw) in
  let leftinput = Chan.create (CInt.dtype ~bits:bw) in
  let command = Chan.create (CInt.dtype ~bits:bw) in
  let rightout = Chan.create (CInt.dtype ~bits:bw) in
  let out = Chan.create (CInt.dtype ~bits:bw) in

  (* instantiate one isntance of the process by calling the function we defined
     above. We must write `kernel.r` and `rightout.w` because the function we
     defined above expects kernel to be the read-end of a port and rightout to
     be the send-end of a port, but kernel and rightout (as created a few lines
     ago) are two-sided channels. *)
  let ir =
    kmac_cjp ~bw ~kernel:kernel.r ~a:a.r ~leftinput:leftinput.r
      ~command:command.r ~rightout:rightout.w ~out:out.w
  in

  (* Now create a Sim instance. We pass it a list of the ports which can be
     read/written by us (the user). We must write `kernel.w.u` (instead of
     `kernel`) and `rightout.r.u` for two reasons. The `w` and `r` specifcy
     whether the port is read-end or send-end. The `u` "untypes" the port. This
     allows us to have heterogeneous types of channels in the same list. *)
  let sim =
    Sim.create ir
      ~user_sendable_ports:[ kernel.w.u; a.w.u; leftinput.w.u; command.w.u ]
      ~user_readable_ports:[ rightout.r.u; out.r.u ]
  in

  (* Now we write the actual meat of the test. The commands run before a given
     Sim.wait are queued. When Sim.wait is called, all the queued commands are
     run. Commands on the same channel are run in the same order as they were
     called, but there is no guarentee in the order between commands on
     different channels. If anything goes wrong (e.g. two parallel reads of the
     same channel, or an attemtp to send a variable on a channel in which it
     does not fit, then the library will print an error. Moreover, if any of the
     queued operations fails to complete, or if any of the queued operations
     reads a value different than expected, the library will also print an
     error. Moreover, if the code takes to long to run, it will print
     "Timne_out". This may indicate an infinite loop, or may indicate you need
     to pass the `max_steps` argument explicitly with a higher value. Otherwise,
     it prints "Ok" *)
  (* Here we send a 6 on the `command` channel, and then attempt to read a 0 on
     the `out` channel. *)
  Sim.send sim command.w (CInt.of_int 6);
  Sim.read sim out.r (CInt.of_int 0);
  (* If the send fails to complete, or the read fails to compelte, or we read a
     value besides 6, this will print an error *)
  Sim.wait' sim ();

  (* Lets do it again, to check that our chp gives the correct answer twice in a
     row. *)
  Sim.send sim command.w (CInt.of_int 6);
  Sim.read sim out.r (CInt.of_int 0);
  Sim.wait' sim ();

  (* When you run this test (with `dune runtest`), it'll run the code and
     compare it to the string inside the `{|` and `|}`. If they're not the same,
     dune will print the diff, and ask you if you want to change the output. If
     you do (e.g. because you changed the test and it should be different now),
     run `dune promote`. This is oparticularly useful when writing new tests.
     Create a new test with no expect output, then run dune and let dune fill in
     the expected output for you!

     This prints out ok twice because we call Sim.wait' twice *)
  [%expect {|
    (Ok ())
    (Ok ()) |}]

(* $MDX part-end *)
