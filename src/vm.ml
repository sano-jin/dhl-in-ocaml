(* vm.ml *)

open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let get_link_name ((ref2link_id, link_id) as env) node_ref =
  second ((^) "L" <. string_of_int) @@
    match List.assq_opt node_ref ref2link_id with
    | None -> (((node_ref, link_id)::ref2link_id, succ link_id), link_id)
    | Some i -> (env, i)

let rec dump_arg ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes || fst !node_ref <> 1
  then first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  else dump_atom false env node_ref 
and dump_atom is_top_level ((dumped_nodes, addr_env) as env) node_ref =
  match snd !node_ref with
  | VMInd y ->
     if is_top_level then 
       first (pair @@ node_ref::dumped_nodes) @@ get_link_name addr_env y
     else first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  | VMAtom (p, xs) ->
     second (fun xs -> p ^ if xs = [] then ""
			   else "(" ^ String.concat ", " xs ^ ")")
     @@ List.fold_left_map dump_arg (first (List.cons node_ref) env) xs

let dump_ind ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes then (env, None)
  else
    second Option.some @@
      if fst !node_ref = 0 then dump_atom true env node_ref
      else
	let (addr_env, link) = get_link_name addr_env node_ref in
	let env = second (const addr_env) env in
	second ((^) @@ link ^ " -> ") @@ dump_atom true env node_ref 
						  
let dump =
  String.concat ". "
  <. List.filter_map id <. snd <. List.fold_left_map dump_ind ([], ([], 0)) 
  <. List.stable_sort (fun r1 r2 -> fst !r1 - fst !r2)

		      
type env = {
  (* all the addresses of the matched atoms on lhs. possibly not indirected from a local link. *)
  local_addrs: node_ref list;
  local2addr: (int * node_ref) list;
  free2addr: (string * node_ref) list;
  free_addr2indeg: (node_ref * int) list;
}

let empty_env =
  {local_addrs = []; local2addr = []; free2addr = []; free_addr2indeg = []}	     

