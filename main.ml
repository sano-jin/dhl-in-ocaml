(* main.ml *)

open Syntax
open Util
open Compile

(* parse : string -> stmt *)
let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

let show str =
  print_string str; str
			  
let test_syntax =
  string_of_proc 0 <. parse <. read_file

let prep_file =
  partit <. parse <. read_file

let test_file file_name i atom_list =
  let CRule (((indegs, _), atoms), _, _) = flip List.nth i @@ snd @@ prep_file file_name in
  Findatom.find_atoms () indegs atom_list atoms 

let test_atom_list =
  Vm.test_atom2atom_list @@
    Vm.TAtom ("test", [
		 Vm.TAtom ("append", [
			      Vm.TAtom ("cons", [
					   Vm.TAtom ("a", []);
					   Vm.TAtom ("nil", [])
					 ]);
			      Vm.TAtom ("cons", [
					   Vm.TAtom ("b", []);
					   Vm.TAtom ("nil", [])
					 ])
			    ])
	       ])

let test_atom_list_nil =
  Vm.test_atom2atom_list @@
    Vm.TAtom ("test", [
		 Vm.TAtom ("append", [
			      Vm.TAtom ("nil", []);
			      Vm.TAtom ("cons", [
					   Vm.TAtom ("a", []);
					   Vm.TAtom ("nil", [])
					 ])
			    ])
	       ])
	     
let dump =
  Vm.dump_atoms test_atom_list

		
let reduce_file file_name i atom_list =
  let rule = flip List.nth i @@ snd @@ prep_file file_name in
  (* Vm.dump_atoms <$> *)
  Eval.reduce atom_list rule 

				
		      
