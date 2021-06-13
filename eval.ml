(* eval.ml *)

open Compile
open Util
open Vm
       
let remove_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [x]))
       
let reduce atom_list (CRule (((lhs_indegs, _), lhs_atoms), (((rhs_indegs, _), rhs_atoms), _), _)) =
  Findatom.find_atoms () lhs_indegs atom_list lhs_atoms
  <&> fun env ->
      let local_addrs = List.map snd env.local2addr in
      let atom_list = set_minus_q atom_list local_addrs in
      List.iter remove_atom local_addrs;
      Pushatom.push_atoms rhs_indegs env.free_addr2indeg env.free2addr rhs_atoms @ atom_list  
