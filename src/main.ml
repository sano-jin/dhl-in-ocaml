(* main.ml *)

open Util
open Compile
open Debug_vm
open Debug_syntax
       
(* read lines from the given file *)
let read_file name : string =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
       close_in ic;
       String.concat "\n" @@ List.rev @@ "" :: acc
  in
  loop []

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
  test_atom2atom_list @@
    TAtom ("test", [
	      TAtom ("append", [
			TAtom ("cons", [
				  TAtom ("a", []);
				  TAtom ("nil", [])
				]);
			TAtom ("cons", [
				  TAtom ("b", []);
				  TAtom ("nil", [])
				])
		      ])
	    ])

let test_atom_list_nil =
  test_atom2atom_list @@
    TAtom ("test", [
	      TAtom ("append", [
			TAtom ("nil", []);
			TAtom ("cons", [
				  TAtom ("a", []);
				  TAtom ("nil", [])
				])
		      ])
	    ])
	     
let reduce_file file_name i atom_list =
  let rule = flip List.nth i @@ snd @@ prep_file file_name in
  (* Vm.dump_atoms <$> *)
  Eval.reduce atom_list rule 

				
		      
