(* main.ml *)

open Syntax
open Util
(* 
open Eval ;; 
*)

(* parse : string -> stmt *)
let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

(*
let run str = 
  eval_stmt [] (parse str)
 *)       

(*
let exec =
  eval_stmt [] <. parse <. read_file
 *)

let show str =
  print_string str; str
			  
let test_syntax =
  string_of_proc 0 <. parse <. read_file

let prep =
  Preprocess.prep <. parse <. read_file

let find_atoms file_name atom_list =
  let Preprocess.Rule' (((atoms, (indegs, _)), _), _) = List.hd @@ snd @@ prep file_name in
  Vm.find_atoms indegs atom_list atoms 

let test =
  flip find_atoms @@
    Vm.test_atom2atom_list @@
      Vm.TAtom ("test", [
		   Vm.TAtom ("append", [
				Vm.TAtom ("cons", [
					     Vm.TAtom ("a", []);
					     Vm.TAtom ("nil", [])
					   ]);
				Vm.TAtom ("cons", [
					     Vm.TAtom ("cons", []);
					     Vm.TAtom ("nil", [])
					   ])
			      ])
		 ])
  
(*


let () =
  match exec Sys.argv.(1) with
  | Some value -> Printf.printf "%s\n" @@ string_of_value value
  | None -> ()
 *)			     
