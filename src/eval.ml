(** eval.ml *)

open Breakdown
open Util
open Vm
       
(** Try to reduce one step with the given atoms and a rule *)				      
let reduce atom_list (BRule (((lhs_indegs, _), lhs_atoms), (((rhs_indegs, _), rhs_atoms), _), redirs)) =
  let+ env = Match.match_ () lhs_indegs atom_list lhs_atoms in
  let local_addrs = List.map snd env.local2addr in
  let redirected_addrs = List.map (flip List.assoc env.free2addr) @@ List.map fst redirs in
  let atom_list = set_minus_q atom_list @@ local_addrs@redirected_addrs in
  List.iter free_atom local_addrs;
  Pushout.push_atoms rhs_indegs env.free_addr2indeg env.free2addr rhs_atoms @ atom_list

(** Try reduce one step with the given atoms and rules *)
let run_once = one_of <. reduce

(** push the initial graph and return their references *)			   
let init_atoms local_indegs inds =
    Pushout.push_atoms (local_indegs, []) [] [] inds
		      
