# dhl-in-ocaml

An interpreter of a language based on graph rewriting in OCaml.

## How to use

1. run `make`
2. run `./dhl`.
3. run `Main.exec "test/append.dhl";;`
   and so on...

## Modules

- main.ml
  - The very main program.
  - Run the `run` function to test.
- eval.ml
  - The evaluator.
  - Run `eval_command` with a `command` and an initial environment (usualy an empty list).
- syntax.ml
  - The definition of the syntax.
- lexer.mll
  - A file to pass to the ocamllex.
- parser.mly
  - A file to pass to the ocamlyacc.
