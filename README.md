# dhl-in-ocaml

An interpreter of a language based on graph rewriting in OCaml.

## How to use

1. run `make`
2. run `./dhl`.
3. run `Main.exec "test/append.dhl";;`
   and so on...

## Project overview

File|blank|comment|code
:-------|-------:|-------:|-------:
parser.mly|27|0|78
findatom.ml|7|3|72
preprocess.ml|15|3|66
compile.ml|14|1|63
util.ml|10|1|51
pushatom.ml|5|1|46
vm.ml|9|2|43
lexer.mll|11|8|34
eval.ml|3|1|14
syntax.ml|2|3|9
--------|--------|--------|--------
SUM:|103|23|476


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
