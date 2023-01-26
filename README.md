## OCaml instalation

First, follow the instructions on this page to set up the ocaml enviroment, but you should use ocaml 4.14 instead of ocaml 4.13.1: http://dev.realworldocaml.org/install.html. The real World OCaml book has a good explanation of good ocaml practices.

Then, install the needed libraries by running `opam install bignum core dune expect_test_helpers_core ocamlfmt`

For now, while this is under development, I like the idea of a monorepo. This means I can fix your code when I change the interfaces. So, just create a new file in the test directory (or several files if you want) and add code there!