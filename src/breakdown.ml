(** breakdown.ml 
    Breakdown argument atoms on right hand side of indirections.
    Check rule condition and reconstruct rule.
*)

open Util
open Alpha
open Link_check

       
type b_arg =
  | BFreeLink of string
  | BLocalLink of int

type b_atom = string * b_arg list

type b_ind =
  | BLocalInd of int * b_atom
  | BFreeInd of string * b_atom
  | BRedir of string * string

type graph = link_info * b_ind list
type b_rule = BRule of graph * (graph * b_rule list) * (string * string) list


(** Breakdown argument atoms *)										 
let rec breakdown_arg env = function
  | AFreeLink x  -> (env, (BFreeLink x, []))
  | ALocalLink (x, _) -> (env, (BLocalLink x, []))
  | AAtom (p, xs) ->
     let ((link_id, local_indegs), (xs, inds)) = breakdown_args env xs in
     (succ link_id, (link_id, 1)::local_indegs),
      (BLocalLink link_id, BLocalInd (link_id, (p, xs))::inds)
and breakdown_args env =
  second (second List.concat <. List.split) <. List.fold_left_map breakdown_arg env

(** Breakdown argument atoms on right hand side of indirection. 
    Indirection from/to local link is not allowed (should be resolved in the former phase, 
    which is not implemented) and raise an error `Redundant indirection`.
*)		 
let breakdown_ind env = function
  | ALocalInd ((x, _), AAtom (p, xs)) ->
     let env, (xs, inds) = breakdown_args env xs in
     env, BLocalInd (x, (p, xs))::inds
  | AFreeInd (x, AAtom (p, xs)) ->
     let env, (xs, inds) = breakdown_args env xs in
     env, BFreeInd (x, (p, xs))::inds
  | AFreeInd (x, AFreeLink y) -> env, [BRedir (x, y)]
  | _ -> failwith @@ "Redundant indirection"    

(** Breakdown argument atoms on right hand side of indirections.
    Returns `(local_indegs, b_ind list)`.
    Takes `link_id` which has returned from the `Alpha.prep`.
 *)
let breakdown_inds = first snd <. second List.concat <.. List.fold_left_map breakdown_ind 


									 
(** Bheck a rule whether meets the conditions *)							    
let check_rule (((lhs_links, _), lhs_rules), ((rhs_links, _), rhs_rules)) =
  if lhs_rules <> [] then failwith @@ "rule(s) on LHS"
  else if rhs_rules <> [] then failwith @@ "rule(s) on RHS is not supported ..."
  else check_link_cond (lhs_links, rhs_links)


(** Bollect redirected free links for an additional indegree checking 
    in non-injective matching.
    UNFINISHED.   
*)							    
let collect_redir = function
  | BRedir (x, y) -> Some (x, y)
  | _ -> None

let collect_redirs = List.filter_map collect_redir
				     
(*				     
let print_int_pair_list l =
  print_string @@ "[" ^ String.concat "; " (List.map (fun (k, v) -> "(" ^ string_of_int k ^ ", " ^ string_of_int v ^ ")") l) ^ "]\n"
 *)
				     
let rec breakdown proc =
  let link_id, (atoms, rules) = alpha proc in
  let (local_indegs, free_indegs), free_names = collect_link_info atoms in
  (* print_int_pair_list local_indegs; *)
  let local_indegs, inds = breakdown_inds (link_id, local_indegs) atoms in
  (* print_int_pair_list local_indegs; *)
  (((local_indegs, free_indegs), free_names), inds), List.map breakdown_rule rules
and breakdown_rule (lhs, rhs) =
  let rule = (breakdown lhs, breakdown rhs) in
  check_rule rule;
  let (lhs, _), (((_, r_graph), _) as rhs) = rule in
  let redirs = List.filter_map collect_redir r_graph in
  BRule (lhs, rhs, redirs)

