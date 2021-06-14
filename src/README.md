# Source programs

## Overview

  | File                               |  LOC | Description                                                         |
  | :--------------------------------- | ---: | :------------------------------------------------------------------ |
  | [findatom.ml](findatom.ml)         |   74 | Performs graph pattern matching                                     |
  | [parser.mly](parser.mly)           |   70 | Defines a grammer for parsing                                       |
  | [util.ml](util.ml)                 |   52 | Some utility functions                                              |
  | [breakdown.ml](breakdown.ml)       |   52 | Check rule conditions and break down atoms                          |
  | [pushatom.ml](pushatom.ml)         |   46 | Generate and push atoms                                             |
  | [vm.ml](vm.ml)                     |   45 | Definition of atom and pretty printer for them                      |
  | [link_check.ml](link_check.ml)     |   44 | Collect link information and check them                             |
  | [main.ml](main.ml)                 |   38 | The main execution loop                                             |
  | [alpha.ml](alpha.ml)               |   34 | Convert local link names to fresh ids and partition atoms and rules |
  | [lexer.mll](lexer.mll)             |   34 | Defies a token for lexing                                           |
  | [debug_syntax.ml](debug_syntax.ml) |   32 | Pretty printer for a parsed input for debugging                     |
  | [debug_vm.ml](debug_vm.ml)         |   24 | Dumper of atoms for debugging                                       |
  | [eval.ml](eval.ml)                 |   17 | The evaluater                                                       |
  | [syntax.ml](syntax.ml)             |    9 | The abstract syntax definition                                      |
  | SUM:                               |  570 |                                                                     |

