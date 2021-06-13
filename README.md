# dhl-in-ocaml

A minimal interpreter of a language based on graph rewriting in OCaml.

## Getting Started
### Prerequisites
- [opam](https://opam.ocaml.org/)
- [dune](https://github.com/ocaml/dune)
  - You can just install `opam` and run `opam install dune`.

### Installation
```bash
git clone https://github.com/sano-jin/dhl-in-ocaml
cd dhl-in-ocaml
dune build
```

## Usage

```bash
dune exec dhl -- test/append.dhl
```

