# Source programs

## Overview

- util.ml
  - Some utility functions

### Compiler

**Syntax analysis**

- syntax.ml
  - The abstract syntax definition
- lexer.mll
  - Defines a token for lexing
- parser.mly
  - Defines a grammar for parsing

**Semantic analysis**

- alpha.ml
  - Convert local link names to fresh ids and partition atoms and rules
- link_check.ml
  - Collect link information and check them
- breakdown.ml
  - Check rule conditions and break down atoms

### VM
- vm.ml
  - Type definition of an atom and pretty printer for its multi-set
- findatom.m
  - Perform graph pattern matching
- pushatom.ml
  - Generate and push atoms
- eval.ml
  - The one step reducer

### Repl
- main.ml
  - File loader and the main execution loop

## LOC

| File                               |  LOC | 
| :--------------------------------- | ---: | 
| [findatom.ml](findatom.ml)         |   74 | 
| [parser.mly](parser.mly)           |   70 | 
| [util.ml](util.ml)                 |   52 | 
| [breakdown.ml](breakdown.ml)       |   52 | 
| [pushatom.ml](pushatom.ml)         |   46 | 
| [vm.ml](vm.ml)                     |   45 | 
| [link_check.ml](link_check.ml)     |   44 | 
| [main.ml](main.ml)                 |   38 | 
| [alpha.ml](alpha.ml)               |   34 | 
| [lexer.mll](lexer.mll)             |   34 | 
| [debug_syntax.ml](debug_syntax.ml) |   32 | 
| [debug_vm.ml](debug_vm.ml)         |   24 | 
| [eval.ml](eval.ml)                 |   17 | 
| [syntax.ml](syntax.ml)             |    9 | 
| SUM:                               |  570 | 
    
