(** vm.ml *)

type vm_atom =
  | VMAtom of string * node_ref ref list
  | VMInd of node_ref ref
 and node_ref = (int * vm_atom) ref  (** (indeg, atom) *)


				
(** An environment for the matching and pushout *)
type env = {
  matched_atoms: node_ref list;
  (** all the addresses of the matched atoms on lhs *)
  
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


(** Traverse indirection atoms and returns the pointed atom.
    There is no worring of circulating indirection
    (if that exists, then the basic design is wrong).
 *)
let rec traverse node_ref_mut =
  let node_ref = !node_ref_mut in
  let indeg = fst !node_ref in
  match snd !node_ref with
  | VMInd y as vm_ind ->
    ( if indeg = 1 then free_atom node_ref (* Free if only I am pointing *)
      else node_ref := (pred indeg, vm_ind) (* Else decrease the indegree by one *)
    );
    let node_ref = traverse y in
    node_ref_mut := node_ref;
    (*    print_string ">>>> traversing indirection atom <<<<\n"; *)
    node_ref
  | VMAtom (_, _) -> node_ref


		       
(** Resolve indirections in an atom list
    Supposed to be called in the end of the program execution.
 *)
let clean_atom_list atom_list =
  let clean_atom node_ref =
    match snd !node_ref with
    | VMInd _ -> failwith "Bug: indirection must not be dereferenced from an atom list"
    | VMAtom (_, xs) -> List.map traverse xs 
  in ignore @@ List.map clean_atom atom_list
