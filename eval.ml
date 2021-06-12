(* eval.ml *)

open Compile
open Util
open Vm
(*
open Findatom
open Pushatom
 *)
       
let remove_atom node_ref =
  match !node_ref with
  | (indeg, VMAtom (p, xs)) -> node_ref := (indeg, VMAtom ("~" ^ p, xs))
  | (indeg, VMInd x) -> node_ref := (indeg, VMAtom ("~->", [x]))
       
let reduce atom_list (CRule (((lhs_indegs, _), lhs_atoms), (((rhs_indegs, free_incidences), rhs_atoms), _), _)) =
  Findatom.find_atoms () lhs_indegs atom_list lhs_atoms
  <&> fun env ->
      let atom_list = set_minus_q atom_list env.local_addrs in
      List.iter remove_atom env.local_addrs;
      dbg_dump_atom_list atom_list;
      let pushed_atoms =
	Pushatom.push_atoms rhs_indegs free_incidences env.free_addr2indeg env.free2addr rhs_atoms in
      print_string "pushed ...\n";
      dbg_dump_atom_list pushed_atoms;
      print_string "with ...\n";
      dbg_dump_atom_list atom_list;
      print_string "...\n";
      let new_atom_list = 
	pushed_atoms @ atom_list in
      print_string "new_atom_list ...\n";
      dbg_dump_atom_list new_atom_list;
      new_atom_list  

  
