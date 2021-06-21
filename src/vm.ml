(** vm.ml *)

open Util
       
type vm_atom =
  | VMAtom of string * node_ref ref list
  | VMInd of node_ref ref
 and node_ref = (int * vm_atom) ref  (** (indeg, atom) *)

let get_link_name ((ref2link_id, link_id) as env) node_ref =
  second ((^) "L" <. string_of_int) @@
    match List.assq_opt node_ref ref2link_id with
    | None -> (((node_ref, link_id)::ref2link_id, succ link_id), link_id)
    | Some i -> (env, i)

let rec dump_arg ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes || fst !node_ref <> 1 
  then
    (* if already have dumped 
     * or the indegree is not 1 and cannot *embed* the atom as an argument *)
    first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  else
    (* *embed* the atom as an argument *)
    dump_atom false env node_ref 
and dump_atom is_top_level ((dumped_nodes, addr_env) as env) node_ref =
  match snd !node_ref with
  | VMInd y ->
     (* If this is the indirection on the *root* of the dumped atom tree,
      *	then we should remember that we have dumped that indirection.
      *	Otherwise, we will just print the link to indirect. *)
     if is_top_level then
       first (pair @@ node_ref::dumped_nodes) @@ get_link_name addr_env !y
     else
       first (pair dumped_nodes) @@ get_link_name addr_env node_ref
  | VMAtom (p, xs) ->
     let (env, xs) = List.fold_left_map dump_arg (first (List.cons node_ref) env) @@ List.map (!) xs in
     (env, p ^ if xs = [] then ""
	       else "(" ^ String.concat ", " xs ^ ")")
     

let dump_ind ((dumped_nodes, addr_env) as env) node_ref =
  if List.memq node_ref dumped_nodes then (env, None)
  else
    second Option.some @@
      if fst !node_ref = 0 then dump_atom true env node_ref
      else
	let (addr_env, link) = get_link_name addr_env node_ref in
	let env = second (const addr_env) env in
	second ((^) @@ link ^ " -> ") @@ dump_atom true env node_ref 


(** A helper function for `tpl_sort` *)  
let rec visit (l, visited) node_ref =
  if List.memq node_ref visited then (l, visited)
  else
    let visited = node_ref::visited in
    let xs = match snd !node_ref with
      | VMAtom (_, xs) -> xs
      | VMInd y -> [y]
    in
    first (List.cons node_ref)
    @@ List.fold_left visit (l, visited) @@ List.map (!) xs 

(** Topological sort *)
let tpl_sort = fst <. List.fold_left visit ([], []) 

(** Pretty printer for printing nodes *)
let dump =
  String.concat ". "
  <. List.filter_map id <. snd <. List.fold_left_map dump_ind ([], ([], 0)) 
  <. tpl_sort
		      

(** An environment for matching and pushout *)
type env = {
  (*  local_addrs: node_ref list; *)
  matched_atoms: node_ref list;
  (** all the addresses of the matched atoms on lhs. 
      possibly not indirected from a local link ? *)
  
  local2addr: (int * node_ref) list;
  free2addr: (string * node_ref) list;
  free_addr2indeg: (node_ref * int) list;
}

let empty_env =
  {matched_atoms = []; local2addr = []; free2addr = []; free_addr2indeg = []}	     


    
(** Free memory fragment of the given address.
    Possibly implemented with `option` type and assign `None`.
 *)
let free_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [x]))


(*
(***********)
(* Delete indirection atoms on atom list *)
let is_not_indir node_ref =
  match snd !node_ref with
  | VMInd _ -> false
  | VMAtom _ -> true

let rec traverse node_ref_mut traversed_indirection =
  let node_ref = !node_ref_mut in
  let indeg = fst !node_ref in
  match snd !node_ref with
  | VMInd y as vm_ind ->
    ( if indeg = 1 then free_atom node_ref (* Free if only I am pointing *)
      else node_ref := (pred indeg, vm_ind) (* Else decrease the indegree by one *)
    );
    let node_ref = traverse y in
    node_ref_mut := node_ref;
    node_ref
  | VMAtom (_, _) -> node_ref
		  
 *)
