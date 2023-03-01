## Motivation
The way a compiler generally works is that it takes a text file, and then does a bunch of computation to turn the text into an [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree). This tree represents the structure of the langauge in the file. For example, the following c code 
```
int c = 0;
c = c * 2;
```
might be turned into the following abstract syntax tree
```
Seq [
  DeclVar (id = 0; name = "c", type = Int, initialization = Some 0);
  Assign (id = 0; expr = Mult (Var (0), Const (2)))
]
```
Along the way, the compiler checks that the text file contains a valid program. For example, this involves checking things like that their are no illegal characters in the file (while lexing), that all pairs of parenticies are balleneced (while parsing), and that strings are never assigned to ints, and that every variable which is used is declared. And, if things go wrong, the compiler must issue a helpful error message. These steps together are called the "front-end" of a compiler.

This, unsurprisingly, means that writing a good compiler front-end is complex. Currently, the Act library has more than 50,000 lines of queit complex code in its front-end! Moreover, the Act langauge still is missing a number of features that we might want. It has some compile time code generation, but it is not as powerful as I would like. Testing is difficult, and their is not great auto-formating or syntax-highlighting support. Moreover, writing a compiler in c is difficult. Compilers involve a lot of short-lived recursive data structures. This means that a olangauge which provides strong type guarentees (like OCaml) and a garbarage-collector (like OCaml) is well suited for this task.

This library takes a different appraoch for writing a front-end for a domain specific langauge. It chooses to embed the language inside an existing langauge. In particular, the user will build an object representing the abstract syntax tree directly at runtime, and then hand that object off to a library function which compilers it down to chp. For example, a simple buffer (with a test included!) would be written as
```
(* This function generated the IR for a simple buffer. It then returns the IR, along
   with the "write end" of the input channel and the "read end" of the output channel. *)
let simple_buffer () =
  let i = Chan.create CInt.dtype_32 in
  let x = Var.create CInt.dtype_32 in
  let o = Chan.create CInt.dtype_32 in
  let ir = Chp.loop [ Chp.read i.r x; Chp.send_var o.w x ] in
  (ir, i.w, o.r)

let%expect_test "test" =
  let ir, i, o = simple_buffer () in
  let sim =
    Sim.create ir ~user_sendable_ports:[ i.u ] ~user_readable_ports:[ o.u ]
  in
  (* Set up a simulation step. We will send the value `3` on `i`, and check that we can read a `3` on `o`. *)
  Sim.send sim i CInt.three;
  Sim.read sim o CInt.three;
  print_s [%sexp (Sim.wait sim () : unit Or_error.t)];
  [%expect {| (Ok ()) |}]
```
This idea came from [Hardcaml](https://github.com/janestreet/hardcaml) and [Halide](https://halide-lang.org/)

Replacing the front-end with this library will have a number of advantages:
1. It will allow users to generate code using the full power of OCaml.
2. It will give use good testing infrastructure for free (by using expect tests).
3. It will give us syntax highlighting and autoformatting for free.
4. It will allow us to drop support for the front-end of the compiler, which is a huge amount of code.
5. It will allow tool development in OCaml, which will signifgantly increate developer velocity.

As an added sweetener, I have implemented a fairly good chp_to_dataflow converter to the tool. The ability to use this will probably compensate for the time it take to learn my tool.

If you want me to help you set up my tool/have any questions at all about OCaml/have any feedback, please get in touch with me (Henry Heffan).

## OCaml instalation

So, now that you are excited to try out this library :), the first thing you need to do is install ocaml.

1. Install opam
2. Run 
```
$ opam switch create 4.14.0
$ eval $(opam env)
$ opam install bignum core dune expect_test_helpers_core mdx merlin ocamlformat
```

3. Clone this repository (e.g. if you have ssh keys installed, `$ git clone git@git.yale.edu:avlsi/ocaml_act.git`)
4. For now, I have not made this library installable. Therefore, you should just create a directory. So, to create a new project, _create a new folder in the cloned directory_ and build your code there.
5. Read the tutorials/examples in the `examples` directory. Start with `examples/README.md`.
