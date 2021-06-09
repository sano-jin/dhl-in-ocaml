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
			  
let exec =
  string_of_proc 0 <. parse <. read_file

let test =
  Preprocess.prep <. parse <. read_file

(*
let () =
  match exec Sys.argv.(1) with
  | Some value -> Printf.printf "%s\n" @@ string_of_value value
  | None -> ()
 *)			     
