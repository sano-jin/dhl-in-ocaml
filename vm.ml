(* vm.ml *)

open Preprocess
open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let get_link ((ref2link_id, link_id) as env) node_ref =
  second ((^) "L" <. string_of_int) @@
    match List.assq_opt node_ref ref2link_id with
    | None -> (((node_ref, link_id)::ref2link_id, succ link_id), link_id)
    | Some i ->
       
       (env, i)

let rec dump_arg ((dumped_nodes, addr_env) as env) node_ref =
  let string_of_addr = fun _ ->
    first (pair dumped_nodes)
    @@ get_link addr_env node_ref
  in
  if List.memq node_ref dumped_nodes
  then string_of_addr ()
  else if fst !node_ref <> 1 then string_of_addr ()
  else dump_inline false env node_ref 
and dump_inline is_top_level ((dumped_nodes, addr_env) as env) node_ref =
  match snd !node_ref with
  | VMInd y ->
     if is_top_level then 
       first (pair @@ node_ref::dumped_nodes) @@ get_link addr_env y
     else first (pair dumped_nodes) @@ get_link addr_env node_ref
  | VMAtom (p, xs) ->
     let (env, xs) =
       List.fold_left_map dump_arg (first (List.cons node_ref) env) xs
     in
     (env, p ^ if xs = [] then ""
	       else "(" ^ String.concat ", " xs ^ ")")
       
let dump_atom ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes then (env, None)
  else
    second Option.some @@
      if fst !node_ref = 0 then dump_inline true env node_ref
      else
	let (addr_env, link) = get_link addr_env node_ref in
	let env = second (const addr_env) env in
	second ((^) @@ link ^ " -> ") @@ dump_inline true env node_ref 
						  
let dump_atoms =
  String.concat ". " <. List.filter_map id <. snd <. List.fold_left_map dump_atom ([], ([], 0)) 

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

let dbg_dump_node_ref addr2link (x, node_ref) =
  let (indeg, atom) = !node_ref in
  "#" ^ string_of_int x ^ " -> " ^ string_of_int indeg ^ ": " ^ dbg_dump_atom addr2link atom
										    
let dbg_dump_atom_list atom_list =
  let addr2link = List.mapi (flip pair) atom_list in
  let link2node_ref = List.mapi pair atom_list in
  print_string @@ String.concat "\n" @@ List.map (dbg_dump_node_ref addr2link) link2node_ref @ ["\n"]
			    
    
type env = {
    (* A set of the addresses which local links have matched.
     * Containes the addresses to the "embedded atom"s. *)
    local_addrs: node_ref list;

    (* A map from the id of matched local links to the matched addresses
     * This does not contain the addresses to the "embedded atom"s on the LHS.
     * Since the local links of them are not known. *)
    local2addr: (int * node_ref) list;

    (* A map from free link names on the LHS to the matched addresses on the heap.  *)
    free2addr: (string * node_ref) list;

    (* A map from the addresses on the heap which the free links have matched
     * to the indegrees that are left.
     * That is, matching free links will consume the indegrees in this map
     * Notice the indegrees should be kept as non-negative value. *)
    free_addr2indeg: (node_ref * int) list;
}

let empty_env =
  {local_addrs = []; local2addr = []; free2addr = []; free_addr2indeg = []}	     
				       
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


							    

						    
