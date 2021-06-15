(** eval.ml *)

open Breakdown
open Util
open Vm
       
(** Free memory fragment of the given address.
    Possibly implemented with `option` type and assign `None`.
 *)
let remove_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [x]))

(** Try to reduce one step with the given atoms and a rule *)				      
let reduce atom_list (BRule (((lhs_indegs, _), lhs_atoms), (((rhs_indegs, _), rhs_atoms), _), _)) =
  let+ env = Findatom.find_atoms () lhs_indegs atom_list lhs_atoms in
  let local_addrs = List.map snd env.local2addr in
  let atom_list = set_minus_q atom_list local_addrs in
  List.iter remove_atom local_addrs;
  Pushatom.push_atoms rhs_indegs env.free_addr2indeg env.free2addr rhs_atoms @ atom_list

(** Try reduce one step with the given atoms and rules *)
let run_once = one_of <. reduce

(** push the initial graph and return their references *)			   
let init_atoms local_indegs inds =
  Pushatom.push_atoms (local_indegs, []) [] [] inds
		      
