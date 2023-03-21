## OCaml background

I strongly recomend reading (or at least skimming) the first few chapters of `http://dev.realworldocaml.org` before reading this tutorial. If you haave not used an ML based langauge, the syntax might be slightly unfamiliar.

## Basic Overview

Most of the code that you will write will probably be CHP. To write CHP code, you need to be familiar with seven modules (For an explination of what a module is, go read chapter one of Real World OCaml!). They are the following
1. CInt  -- This represents a nonnegative integer of arbitrary width. It supports most math operations, but requires a specific bitwidth for subtracktion (if it can underflow) and bitwise-not.
2. CBool  -- A light wrapper about a CInt that is 1 bit wide.
3. DType  -- This represents a datatype that can be help in a chp variable or channel. It hold a way to encode and decode the value to a CInt. It has a fixed bitwidth. 
5. Var  -- This represents a chp variable. When created, it must be passed a particular dtype. All later assignments will be checked to see they match that DType. 
4. Expr  -- This represents some mathamatical expression. It usually involves reading several variables.
6. Chan  -- This represents a chp channel. Just like a Var, when created, it must be passed a particular dtype. All later sends will be checked to see they match that DType. 
7. Chp  -- This represents individual statements in chp, as well as control flow. For example, Chp.par creates a statement mad3e of several statements in parallel.

I think an example will be useful

<!-- $MDX file=simple_buffer.ml,part=simple_buffer_example -->
```ocaml
(* This function generated the IR for a simple buffer. It then returns the IR,
   along with the "write end" of the input channel and the "read end" of the
   output channel. *)
let simple_buffer () =
  let i = Chan.create CInt.dtype_32 in
  let x = Var.create CInt.dtype_32 in
  let o = Chan.create CInt.dtype_32 in
  let chp = Chp.loop [ Chp.read i.r x; Chp.send_var o.w x ] in
  (chp, i.w, o.r)

let%expect_test "test" =
  (* Instantiate the buffer *)
  let chp, i, o = simple_buffer () in
  (* create a simulation *)
  let sim =
    Sim.simulate_chp chp ~user_sendable_ports:[ i.u ]
      ~user_readable_ports:[ o.u ]
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
```

## A Second Example


Take a basic example:

```
defproc<pint bw> kmac_cjp ( chan?(int<bw>) A;                    //input activation
                            chan?(int<bw>) kernel;               //input kernel
                            chan?(int<bw>) rightinput;           //activation coming form right PE
                            chan?(int<bw>) command;              //command channel input 
                            chan!(int<bw>) leftout;              //output for pushing activation left
                            chan!(int<bw>) out ){                //output to memory
   
    int<bw> inputx;
    int<bw> tininputx;
    int<bw> toutinputx;
    int<bw> outputx;
    int<bw> kernelV;
    int<bw> c;
    bool dummy_bool;
    int<bw> dummy_bidwidth;

    chp {
           outputx:=0; 

           *[command?c;
                [  c = 0 ->  outputx    := tininputx * kernelV + outputx ;         // MAC
                [] c = 1 ->  toutinputx := tininputx; rightout!toutinputx, leftinput?tininputx; 
                [] c = 5 ->  tininputx := inputx;
                [] c = 6 ->  out!outputx; 
                [] c = 7 ->  kernel?kernelV;
                [] c = 8 ->  A?inputx; log(c,") Reading Activation: ",inputx );
                [] c = 9 ->  outputx := 0;
                ]                  
            ]
        }
}
```

Here is how that same process might look as (heavily commented) OCaml code:
<!-- $MDX file=lukas_example.ml,part=lukas_example -->
```ocaml
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
      (* Now we will add a select statement, using the function `Chp.select_imm
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
  let chp =
    kmac_cjp ~bw ~kernel:kernel.r ~a:a.r ~leftinput:leftinput.r
      ~command:command.r ~rightout:rightout.w ~out:out.w
  in

  (* Now create a Sim instance. We pass it a list of the ports which can be
     read/written by us (the user). We must write `kernel.w.u` (instead of
     `kernel`) and `rightout.r.u` for two reasons. The `w` and `r` specifcy
     whether the port is read-end or send-end. The `u` "untypes" the port. This
     allows us to have heterogeneous types of channels in the same list. *)
  let sim =
    Sim.simulate_chp chp
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
```

## Building complex programs

The above to examples illustrate how to write small parts of a program. This library provides a way to stitch together small parts of a program into a larger whole.

First, a block of Chp code may be pacakged together into a process. A collection of processes may also be grouped together into a larger process. In this case, the processes

A process has "input ports" and "output ports", and some chp code. The invariants on a Process are that (1) it does not share varaibles with any other process, (2) if it reads a channel, no other (non-child) process reads that channel, (3) if it writes a channel, no other (non-child) process writes that channel, (4) if it (or a child process) reads a channel but neither it nor any child process writes that channel, that channel is listed as an "input port", and (5) if it (or a child process) writes a channel but neither it nor any child process reads that channel, that channel is listed as an "output port ".

If Chp code meets stronger requirements, it may be packaged as a "dataflow-able." This changes the semantics of the interface of the process. In particular, it does 3 things to the process. First, it creates buffers of unspecified length on every input and output port. Second, it promisses that every input and output port will receive an infinite stream of tokens. Third, it allows the program to have "premonitions" about which tokens it will receive, and execute accordingly. 

A program is basically just a single processes with subprocesses. Its inputs and output ports are "top-level io ports", and will be available for user iteration, both in the exported code and simulation. Simulation of a program is done with `Sim.simulate top_process`, and `Sim.simulate_chp` is merely light wrapper around `Process.of_chp` followed by `Sim.simulate`.

Here is an example of building some processes and process clusters.

<!-- $MDX file=recursive_buffer.ml,part=recursive_buffer_example -->
```ocaml
let split ~dtype i1 o1 o2 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.read i1 var1;
      Chp.if_else
        Expr.(var b1)
        [ Chp.send_var o1 var1 ]
        [ Chp.send_var o2 var1 ];
      CBool.Chp.toggle b1;
    ]
  |> Process.of_chp ~iports:[ i1.u ] ~oports:[ o1.u; o2.u ]

let merge ~dtype i1 i2 o1 =
  let var1 = Var.create dtype in
  let b1 = Var.create CBool.dtype ~init:false in
  Chp.loop
    [
      Chp.if_else Expr.(var b1) [ Chp.read i1 var1 ] [ Chp.read i2 var1 ];
      Chp.send o1 Expr.(var var1);
      CBool.Chp.toggle b1;
    ]
  |> Process.of_chp ~iports:[ i1.u; i2.u ] ~oports:[ o1.u ]

let rec buff ~depth ~dtype i1 o1 =
  (if depth <= 0 then failwith "depth too low"
   else if Int.equal depth 1 then
     let chan1 = Chan.create dtype in
     let chan2 = Chan.create dtype in
     [ split ~dtype i1 chan1.w chan2.w; merge ~dtype chan1.r chan2.r o1 ]
   else
     let chan1a = Chan.create dtype in
     let chan1b = Chan.create dtype in
     let chan2a = Chan.create dtype in
     let chan2b = Chan.create dtype in

     [
       split ~dtype i1 chan1a.w chan2a.w;
       buff ~dtype ~depth:(depth - 1) chan1a.r chan1b.w;
       buff ~dtype ~depth:(depth - 1) chan2a.r chan2b.w;
       merge ~dtype chan1b.r chan2b.r o1;
     ])
  |> Process.of_procs ~iports:[ i1.u ] ~oports:[ o1.u ]

let buff ~depth ~dtype =
  let i = Chan.create dtype in
  let o = Chan.create dtype in
  let top_process = buff ~depth ~dtype i.r o.w in
  (top_process, i.w, o.r)

let%expect_test "test" =
  let process, i, o = buff ~depth:3 ~dtype:(CInt.dtype ~bits:9) in
  let sim = Sim.simulate process in

  let l =
    [ 1; 5; 9; 33; 123; 258; 500; 7; 9; 4; 5 ] |> List.map ~f:CInt.of_int
  in
  List.iter l ~f:(fun v -> Sim.send sim i v);
  Sim.wait' sim ();
  List.iter l ~f:(fun v -> Sim.read sim o v);
  Sim.wait' sim ();
  [%expect {|
    (Ok ())
    (Ok ()) |}]
```

## Exporting chp and dataflow code

Great! So now we know how to build complex programs. All that remains is exporting them into act code. This is done with the `Compiler` module. First you "compile" the program, and then you export it. You may also simulate the compiled program. While compiling is when chp is converted into dataflow. An example will demonstrate this well:


<!-- $MDX file=colatz.ml,part=colatz_example -->
```ocaml
let i = Chan.create CInt.dtype_32
let o = Chan.create CInt.dtype_32

let ir =
  let var0 = Var.create CInt.dtype_32 ~init:(CInt.of_int 123456) in
  let counter = Var.create CInt.dtype_32 ~init:(CInt.of_int 1) in
  Chp.loop
    [
      Chp.assign counter Expr.zero;
      Chp.read i.r var0;
      Chp.while_loop
        CInt.E.(ne (var var0) (of_int 1))
        [
          CInt.Chp.assign counter
            CInt.E.(var counter |> add (of_int 1))
            ~overflow:Cant;
          Chp.if_else
            CInt.E.(mod_ (var var0) (of_int 2) |> eq (of_int 0))
            [ Chp.assign var0 CInt.E.(div (var var0) (of_int 2)) ]
            [
              CInt.Chp.assign var0
                CInt.E.(var var0 |> mul (of_int 3) |> add (of_int 1))
                ~overflow:Cant;
            ];
        ];
      Chp.send_var o.w counter;
    ]

let%expect_test "colatz - chp" =
  let sim =
    Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
      ~user_readable_ports:[ o.r.u ] ~to_:`Chp
    |> Compiler.sim
  in
  Sim.send sim i.w (CInt.of_int 4);
  Sim.read sim o.r (CInt.of_int 2);
  Sim.wait' sim ();

  Sim.send sim i.w (CInt.of_int 12345);
  Sim.read sim o.r (CInt.of_int 50);
  Sim.wait' sim ();

  Sim.send sim i.w (CInt.of_int 13579753);
  Sim.read sim o.r (CInt.of_int 166);
  Sim.wait' ~max_steps:1000000 sim ();

  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ()) |}]

let%expect_test "colatz - dataflow" =
  let sim =
    Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
      ~user_readable_ports:[ o.r.u ] ~to_:`Dataflow
    |> Compiler.sim
  in
  Sim.send sim i.w (CInt.of_int 4);
  Sim.read sim o.r (CInt.of_int 2);
  Sim.wait' ~max_steps:1000000 sim ();

  Sim.send sim i.w (CInt.of_int 12345);
  Sim.read sim o.r (CInt.of_int 50);
  Sim.wait' ~max_steps:1000000 sim ();

  Sim.send sim i.w (CInt.of_int 13579753);
  Sim.read sim o.r (CInt.of_int 166);
  Sim.wait' ~max_steps:1000000 sim ();

  [%expect {|
    (Ok ())
    (Ok ())
    (Ok ()) |}]

let%expect_test "colatz - chp - export" =
  Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
    ~user_readable_ports:[ o.r.u ] ~to_:`Chp
  |> Compiler.export |> printf "%s";
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
      int<32> v1;
    chp {
    (v1 := 123456); (v0 := 1); ( *[ ( [true] ); (v0 := 0); (C0?v1); ( [true] ); ([bool(int(int((v1) != 1) = 0)) ->  [true]  [] bool(int((v1) != 1)) ->  *[ ( [true] ); (v0 := (1 + (v0))); ( [true] ); ([bool(int(0 = ((v1) % 2))) -> ( [true] ); (v1 := ((v1) / 2)) [] bool(int(int(0 = ((v1) % 2)) = 0)) -> ( [true] ); (v1 := (1 + (3 * (v1))))]); ( [true] ) <- bool(int((v1) != 1)) ] ]); ( [true] ); (C1!((v0))) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]

let%expect_test "colatz - dataflow - export" =
  Compiler.compile_chp ir ~user_sendable_ports:[ i.w.u ]
    ~user_readable_ports:[ o.r.u ] ~to_:`Chp
  |> Compiler.export |> printf "%s";
  [%expect
    {|
    defproc proc_0(chan!(int<32>) C0; chan?(int<32>) C1) {

      int<32> v0;
      int<32> v1;
    chp {
    (v1 := 123456); (v0 := 1); ( *[ ( [true] ); (v0 := 0); (C0?v1); ( [true] ); ([bool(int(int((v1) != 1) = 0)) ->  [true]  [] bool(int((v1) != 1)) ->  *[ ( [true] ); (v0 := (1 + (v0))); ( [true] ); ([bool(int(0 = ((v1) % 2))) -> ( [true] ); (v1 := ((v1) / 2)) [] bool(int(int(0 = ((v1) % 2)) = 0)) -> ( [true] ); (v1 := (1 + (3 * (v1))))]); ( [true] ) <- bool(int((v1) != 1)) ] ]); ( [true] ); (C1!((v0))) <- bool(1) ] )
    }
    }


    defproc main(chan?(int<32>) user_i0;chan!(int<32>) user_o0) {

      chan(int<32>) c0;
      chan(int<32>) c1;
    proc_0 proc_0_ (c0,c1);
    c0 = user_i0;
    c1 = user_o0;
    } |}]
```
