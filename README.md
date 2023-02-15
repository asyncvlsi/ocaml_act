## OCaml instalation

First, follow the instructions on this page to set up the ocaml enviroment, but you should use ocaml 4.14 instead of ocaml 4.13.1: http://dev.realworldocaml.org/install.html. The real World OCaml book has a good explanation of good ocaml practices.

Then, install the needed libraries by running `opam install bignum core dune expect_test_helpers_core ocamlfmt`

For now, while this is under development, I like the idea of a monorepo. This means I can fix your code when I change the interfaces. So, just create a new file in the test directory (or several files if you want) and add code there!




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
First start by creating a function(remember the space  before the tilde), and importing the ocaml libraries:
```
open! Core
open! Act

let kmac_cjp ~bw ~kernel ~A ~rightinput ~command ~leftout ~out = ()
```
This creates the procedure, roughly equivalent to the defproc of chp(this is an empty defproc). There is no distinction between a templated variable and a in chp variable as the library performs the expansion and instantiation automatically. 

Note that as currently designed, there is no check on the input channel bitwidths automatically, and so you need to check it manually. Note that the bitwidth interface is still under development and may be subject to change.
```
let kmac_cjp ~bw ~kernel ~A ~rightinput ~command ~leftout ~out = (
    assert (List.all_equal [bw; kernel.bw; rightinput.bw; A.bw; command.bw; command.bw; leftout.bw; out.bw]);

)
```
Now add the instantiation of the various variables:
```
let kmac_cjp ~bw ~kernel ~A ~rightinput ~command ~leftout ~out = (
    assert (List.all_equal [bw; kernel.bw; rightinput.bw; A.bw; command.bw; command.bw; leftout.bw; out.bw]);
    let val_dtype = CInt.dtype ~bits: bw in
    let inputx = Var.create val_dtype in
    let tininputx = Var.create val_dtype in
    let toutinputx = Var.create val_dtype in
    let outputx = Var.create val_dtype ~init:CInt.0 in
    let kernelV = Var.create val_dtype in
    let c = Var.create val_dtype in 
    let dummy_bool = Var.create CBool.dtype in 
    let dummy_inline_bitwidth = Var.create (CInt.dtype ~bits) in () 
)
```
Now this is the startup bit, now we write some CHP. We start by writing the wrapper loop, with the outputx := 0 covered by the init optional argument for `Var.create`, and write the first read. 
```
let kmac_cjp ~bw ~kernel ~A ~rightinput ~command ~leftout ~out = (
    assert (List.all_equal [bw; kernel.bw; rightinput.bw; A.bw; command.bw; command.bw; leftout.bw; out.bw]);
    let val_dtype = CInt.dtype ~bits: bw in
    let inputx = Var.create val_dtype in
    let tininputx = Var.create val_dtype in
    let toutinputx = Var.create val_dtype in
    let outputx = Var.create val_dtype ~init:CInt.0 in
    let kernelV = Var.create val_dtype in
    let c = Var.create val_dtype in 
    let dummy_bool = Var.create CBool.dtype in 
    let dummy_inline_bitwidth = Var.create (CInt.dtype ~bits) in 
    CHP.loop [CHP.read command c]
)
```
The CHP.loop function expects a list of CHP code which will be executed in sequence. The `CHP.read <chan> <var>` expects a readable channel, and it reads to a variable. 

Now we will add a select statement, `CHP.select_immd <statements> ~else_:<statement>`. This is a function which expects a list of tuples of the type `(<expression guard>, <CHP statement>`), and if no guards are true, excutes the named argument else. (Note that in OCAML, tuples use , while lists use ;). For the following, we'll use None for the else. The syntax `CInt.E.(<statements>)` opens the local module `CInt.E` and exposes the functions inside for use in the guard. This is generally used for working with expressions as opposed to statements.

The important functions you should use are within the `CInt.E` are the equality functions which create the guards. 
```
eq <expression> <expression>
neq <expression> <expression>
lt <expression> <expression>
le <expression> <expression>
gt <expression> <expression>
ge <expression> <expression>
```

There are also the functions that convert variables and constants to expressions so they can be used in these functions. 
```
var <variable>
const <cint type>
cint <integer constant like 8>
```
let kmac_cjp ~bw ~kernel ~A ~rightinput ~command ~leftout ~out = (
    assert (List.all_equal [bw; kernel.bw; rightinput.bw; A.bw; command.bw; command.bw; leftout.bw; out.bw]);
    let val_dtype = CInt.dtype ~bits: bw in
    let inputx = Var.create val_dtype in
    let tininputx = Var.create val_dtype in
    let toutinputx = Var.create val_dtype in
    let outputx = Var.create val_dtype ~init:CInt.0 in
    let kernelV = Var.create val_dtype in
    let c = Var.create val_dtype in 
    let dummy_bool = Var.create CBool.dtype in 
    let dummy_inline_bitwidth = Var.create (CInt.dtype ~bits) in 
    CHP.loop [CHP.read command c; 
        CHP.select_imm [
            (CInt.E.(eq (var c) (cint 0)), CHP.assign outputx CInt.E.(add (mul (var tinputx) (var kernelV)) (var outputx)));
            (CInt.E.(eq (var c) (cint 1)), 
            CHP.seq [
                CHP.assign toutinputx CInt.E.(var tininputx);CHP.par [CHP.send_var rightout toutinputx; CHP.read leftinput tininputx] ]);
            (CInt.E.(eq (var c) (cint 5)), CHP.assign tininputx CInt.E.(var inputx));
            (CInt.E.(eq (var c) (cint 6)), CHP.send_var out outputx;
            (CInt.E.(eq (var c) (cint 7)), CHP.read kernel kernelV);
            (CInt.E.(eq (var c) (cint 8)), CHP.read A inputx);
            (CInt.E.(eq (var c) (cint 9)), CHP.assign outputx CInt.E.(cint 0));
        ] ~else_:None
    ]
)
```
This is a complete implementation of the earlier code. It assumes that the variables passed in are of the correct type.

Now we can write a test for the procedure.

We will use the OCAML expect_test library.

This has the basic format:
```
let%expect_test "<testname>" = 
<code>;
[%expect {| <output> |}] 
```
When you run this test, it'll run the code and compare it to output. 
If they're not the same, it'll give you an error - it does a string compare.

For our code we can write the following
```
let%expect_test "kmac_cjp_test_1" = 
    let bw = 8 in
    let kernel = Chan.create (CInt.dtype ~bits:bw) in
    let A = Chan.create (CInt.dtype ~bits:bw) in
    let rightinput = Chan.create (CInt.dtype ~bits:bw) in
    let command = Chan.create (CInt.dtype ~bits:bw) in
    let leftout = Chan.create (CInt.dtype ~bits:bw) in
    let out = Chan.create (CInt.dtype ~bits:bw) in
    let ir = kmac_cjp ~bw:bw ~kernel:kernel ~rightinput:rightinput ~command: command ~leftout:leftout ~out:out in
    ()
```
The issue with this is that the code expects read only and write only channels, so we have to specify that by using .r or .w after the channel name.

```
let%expect_test "kmac_cjp_test_1" = 
    let bw = 8 in
    let kernel = Chan.create (CInt.dtype ~bits:bw) in
    let A = Chan.create (CInt.dtype ~bits:bw) in
    let rightinput = Chan.create (CInt.dtype ~bits:bw) in
    let command = Chan.create (CInt.dtype ~bits:bw) in
    let leftout = Chan.create (CInt.dtype ~bits:bw) in
    let out = Chan.create (CInt.dtype ~bits:bw) in
    let ir = kmac_cjp ~bw:bw ~kernel:kernel.r ~rightinput:rightinput.r ~command:command.r ~leftout:leftout.w ~out:out.w in
    ()
```

This should run.  

Run your test to check if the generated code is well typed by running:
```
dune runtest
```

Note: Run this test inside a file inside test. 
Now create a Sim instance and pass it a list of the ports you want to be able to 
read and write. 
Note write <chan>.w.u to indicate that it would be user writable and <chan>.r.u 
to indicate that it would be user readable.
```
let%expect_test "kmac_cjp_test_1" = 
    let bw = 8 in
    let kernel = Chan.create (CInt.dtype ~bits:bw) in
    let A = Chan.create (CInt.dtype ~bits:bw) in
    let rightinput = Chan.create (CInt.dtype ~bits:bw) in
    let command = Chan.create (CInt.dtype ~bits:bw) in
    let leftout = Chan.create (CInt.dtype ~bits:bw) in
    let out = Chan.create (CInt.dtype ~bits:bw) in
    let ir = kmac_cjp ~bw:bw ~kernel:kernel.r ~rightinput:rightinput.r ~command:command.r ~leftout:leftout.w ~out:out.w in
    let sim = Sim.create ir ~user_sendable_ports: [kernel.w.u; rightinput.w.u; command.w.u; leftout.w.u]
    ~user_readable_ports: [leftout.r.u; out.r.u] in
    Sim.wait' sim();
    Sim.send command.w (CInt.of_int 6);
    Sim.read out.r (CInt.of_int 0);
    Sim.wait' sim();
    [%expect {| <output> |}];
```
So how this works is: the simulation is run on Sim.wait. 
The commands before Sim.wait are queued and run randomly if they are on different channels and in series if they're on the same channel.
Seperate the commands with semicolons.
To send to a channel, you need to specify that the channel is writable with .w. 
To read a channel you need to specify that the channel is readable with .r.



CHP Commands:
== Var ==
How to instantiate a var:



== CHP.assign ==

`CHP.assign v e`  is a stand in for the following chp:
```
v := e
```
`v` is of type `'dtype Var.t`. Basically this is one of the following types, wrapped in a Var.t class. 


== CHP.read ==

CHP.send

CHP.log

CHP.seq

CHP.par

CHP.while_loop

CHP.imm_select
