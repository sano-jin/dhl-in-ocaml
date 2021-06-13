(* main.ml *)

open Util
open Compile
       
(* read lines from the given file *)
let read_file name =
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
  @@ Lexing.from_string str

let prep_file =
  partit <. parse <. read_file

let rec run_many dumper i rules atoms =
  print_string @@ string_of_int i ^ ": " ^ dumper atoms ^ "\n";
  run_many dumper (succ i) rules <$> Eval.run_once atoms rules |> maybe atoms 

let run_file dumper file_name  =
  match file_name |> read_file |> parse |> partit with
  | ((((local_indegs, []), []), inds), rules) ->
     let final_state = run_many dumper 0 rules @@ Eval.init_atoms local_indegs @@ List.rev inds in
     print_string @@ "Final state: " ^ dumper final_state ^ "\n"
  | _ -> failwith "free links are not allowed in the initial graph"

let valid_options = ["-dbg"]  
let () =
  match List.tl @@ Array.to_list Sys.argv with
  | [] -> failwith @@ "no input file"
  | file_name::options ->
     match set_minus options valid_options with
     | (_::_) as invalid_options ->
	failwith @@ "invalid options " ^ String.concat ", " invalid_options
     | [] ->
	let dumper = 
	  if List.mem "-dbg" options then Debug_vm.dbg_dump
	  else Vm.dump
	in run_file dumper file_name
