(* vm.ml *)

open Preprocess
open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let null_ptr: node_ref = ref (0, VMAtom ("Null", []))

let 			     
			     
let string_of_vm_atom (dumped_nodes, ref2link_id (* the number corresponds to the printed link id *)) node_ref =
  if List.memq node_ref dumped_nodes
     || fst !node_ref <> 1 
  then pair dumped_atoms @@
	 match List.nth_opt ref2link_id with
	 | None -> ref2link_id @ [atom]
	 | Some i -> "L" ^ string_of_int i
  else match snd !node_ref with
       | VMInd node_ref =
	   
	   string_of_vm_atom (dumped_nodes, ref2link_id (* the number corresponds to the printed link id *)) node_ref =
       | VMAtom (p, xs) as atom ->
     
    
			     
let string_of_atom_list atom_list =
  atom_list
			     
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

let update_ref f r = r := f !r

let test_atom2atom_list =
  let rec helper acc = function
    | TAtom (p, xs) ->
       let (acc, xs) = List.fold_left_map helper acc xs in
       let node_ref = ref (1, VMAtom (p, xs)) in
       (node_ref::acc, node_ref)
  in
  fst <. second @@ update_ref @@ first @@ const 0 <. helper []


							    

						    
