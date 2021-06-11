(* compile.ml *)

open Preprocess
open Util
       
type c_arg =
  | CFreeLink of string
  | CLocalLink of int

type c_atom = string * c_arg list

type c_ind =
  | CLocalInd of int * c_atom
  | CFreeInd of string * c_atom
  | CRedir of string * string

type c_inds = c_ind list
type c_rule = CRule of (link_info * c_inds) * ((link_info * c_inds) * c_rule list) * (string * string) list
 
let rec partit_arg ((link_id, local_indegs) as env) = function
  | PFreeLink x  -> (env, (CFreeLink x, []))
  | PLocalLink (x, _) -> (env, (CLocalLink x, []))
  | PAtom (p, xs) ->
     let ((link_id, local_indegs), (links, inds)) = partit_args env xs in
     (succ link_id, (link_id, 1)::local_indegs),
      (CLocalLink link_id, CLocalInd (link_id, (p, links))::inds)
and partit_args env =
  second (second List.concat <. List.split) <. List.fold_left_map partit_arg env

let rec partit_ind env = function
  | PLocalInd ((x, _), PAtom (p, xs)) ->
     let (env, (links, inds)) = partit_args env xs in
     (env, CLocalInd (x, (p, links))::inds)
  | PFreeInd (x, PAtom (p, xs)) ->
     let (env, (links, inds)) = partit_args env xs in
     (env, CFreeInd (x, (p, links))::inds)
  | PFreeInd (x, PFreeLink y) ->
     (env, [CRedir (x, y)])
  | _ -> failwith @@ "Redundant indirection"    

let partit_inds = second List.concat <.. List.fold_left_map partit_ind 

let check_link_cond ((lhs_indegs, lhs_free_incidences),
		     (rhs_indegs, rhs_free_incidences)) =
  let free_names = List.map fst <. snd in
  let unbound_rhs_links = set_minus (free_names rhs_indegs) (free_names lhs_indegs) in
  if unbound_rhs_links <> [] then
    failwith @@ "link(s) " ^ String.concat ", " unbound_rhs_links ^ " on RHS has/have not appeared on LHS"
  else ();
  let unredired = set_minus lhs_free_incidences rhs_free_incidences in
  if unredired <> [] then 
    failwith @@ "link(s) " ^ String.concat ", " unredired ^ " on LHS is/are not redirected on RHS"
  else ()

let check_rule (((lhs_links, _), lhs_rules), ((rhs_links, _), rhs_rules)) =
  if lhs_rules <> [] then failwith @@ "rule(s) on LHS"
  else if rhs_rules <> [] then failwith @@ "rule(s) on RHS is not supported ..."
  else check_link_cond (lhs_links, rhs_links)

let collect_redir = function
  | CRedir (x, y) -> Some (x, y)
  | _ -> None

let collect_redirs = List.filter_map collect_redir
	   
let rec partit proc =
  let link_id, (atoms, rules) = second partitionEithers @@ prep_atoms [] 0 proc in
  let ((local_indegs, _), _) as link_info = collect_link_info atoms in
  (first (flip flip link_info @@ first <. first <. const)
   @@ first snd @@ partit_inds (link_id, local_indegs) atoms,
   List.map partit_rule rules)
and partit_rule (lhs, rhs) =
  let rule = (partit lhs, partit rhs) in
  check_rule rule;
  let (lhs, (((_, r_graph), _) as rhs)) = first fst rule in
  let redirs = List.filter_map collect_redir r_graph in
  CRule (lhs, rhs, redirs)

