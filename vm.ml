(* vm.ml *)

open Preprocess
open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let null_ptr: node_ref = ref (0, VMAtom ("Null", []))

let get_addr ((ref2link_id, link_id) as env) node_ref =
  match List.assq_opt node_ref ref2link_id with
  | None -> (((node_ref, link_id)::ref2link_id, succ link_id), link_id)
  | Some i -> (env, i)

let get_str_addr env =
  second ((^) "L" <. string_of_int) <. get_addr env

let rec dump_arg ((dumped_nodes, addr_env) as env) node_ref =
  let string_of_addr dumped_nodes =
    first (pair dumped_nodes)
    @@ get_str_addr addr_env node_ref
  in
  if List.memq node_ref dumped_nodes
  then string_of_addr dumped_nodes
  else if fst !node_ref <> 1 then string_of_addr @@ node_ref::dumped_nodes
  else dump_inline env node_ref
and dump_inline ((dumped_nodes, addr_env) as env) node_ref =
  match snd !node_ref with
  | VMInd node_ref ->
     first (pair dumped_nodes) @@ get_str_addr addr_env node_ref
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
      if fst !node_ref = 0 then dump_inline env node_ref
      else
	let (addr_env, link) = get_str_addr addr_env node_ref in
	let env = second (const addr_env) env in
	second ((^) @@ link ^ " -> ") @@ dump_inline env node_ref
						  
let dump_atoms =
  String.concat ". " <. List.filter_map id <. snd <. List.fold_left_map dump_atom ([], ([], 0)) 

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


							    

						    
