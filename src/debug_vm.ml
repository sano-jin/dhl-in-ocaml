(* vm.ml *)

open Util
open Vm
       
let dbg_addr = ref (-1)
									
let rec dbg_dump_ref addr2link node_ref =
  (^) "#" @@ string_of_int @@
    match List.assq_opt node_ref addr2link with
    | None ->
       let x = !dbg_addr in
       let (indeg, atom) = !node_ref in
       print_string ("segfault... " ^ string_of_int x
		     ^ " -> " ^ string_of_int indeg ^ ": " ^ dbg_dump_atom addr2link atom ^ "\n");
       update_ref pred dbg_addr;
       x
    | Some s -> s
and dbg_dump_atom addr2link = function  
  | VMAtom (p, xs) -> p ^ " (" ^ String.concat ", " (List.map (dbg_dump_ref addr2link) xs) ^ ")"
  | VMInd x -> dbg_dump_ref addr2link x

let dbg_dump_ind addr2link (x, node_ref) =
  let (indeg, atom) = !node_ref in
  "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ ": " ^ dbg_dump_atom addr2link atom
										    
let dbg_dump atom_list =
  let addr2link = List.mapi (flip pair) atom_list in
  let link2node_ref = List.mapi pair atom_list in
  print_string @@ String.concat "\n" @@ List.map (dbg_dump_ind addr2link) link2node_ref @ ["\n"]
			    
type test_atom =
  | TAtom of string * test_atom list

let test_atom2atom_list =
  let rec helper acc = function
    | TAtom (p, xs) ->
       let (acc, xs) = List.fold_left_map helper acc xs in
       let node_ref = ref (1, VMAtom (p, xs)) in
       (node_ref::acc, node_ref)
  in
  fst <. second @@ update_ref @@ first @@ const 0 <. helper []
