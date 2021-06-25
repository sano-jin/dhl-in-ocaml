#!/bin/bash

test () {
    echo "testing $1"
    opam exec -- dune exec dhl -- ../example/$1.dhl -t > tmp/output_$1.log
    opam exec -- patdiff tmp/output_$1.log expected/expected_output_$1.log || exit
}
echo testing
mkdir tmp
test append
test test
test test2
test test3
test test4
test test5

rm -rf tmp
