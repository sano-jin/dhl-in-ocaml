#!/bin/sh
make
./dhl
open Main;;
test "test/append.dhl" 0 test_atom_list;;
test "test/append.dhl" 1 test_atom_list_nil;;
test "test/append.dhl" 1 test_atom_list;;
test "test/append.dhl" 0 test_atom_list_nil;;
exit 0;;
