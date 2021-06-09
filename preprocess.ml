(* preprocess.ml *)

open Syntax
open Util

type local_link = int * string  (* (link_id, link_name_for_debugging) *)
		       
type arg' =
  | FreeLink of string
  | LocalLink of local_link
  | Atom' of string * arg' list

type root_atom =
  | LocalInd of local_link * arg'
  | FreeInd of string * arg'
			  
type atoms = root_atom list
type link_info = ((int * int) list * (string * int) list ) * string list
type rule = Rule' of (((atoms * link_info) * rule list) * ((atoms * link_info) * rule list))

						      
let rec prep_arg env = function
  | Atom (p, xs) -> Atom' (p, List.map (prep_arg env) xs)
  | Link x -> match List.assoc_opt x env with
	      | None -> FreeLink x
	      | Some i -> LocalLink (i, x)
			  
let rec prep_atoms env link_id = function
  | Zero -> (link_id, [])
  | Ind  (None, p) ->
     (succ link_id, [Either.Left (LocalInd ((link_id, ""), prep_arg env p))])
  | Ind  (Some x, p) ->
     (link_id,
      [Either.Left (
	   match List.assoc_opt x env with
	   | None -> FreeInd (x, prep_arg env p)
	   | Some i -> LocalInd ((i, x), prep_arg env p)
	 )])
  | Mol  (p, q) ->
     second List.concat @@ List.fold_left_map (prep_atoms env) link_id [p; q] 
  | New  (x, p) ->
     prep_atoms ((x, link_id)::env) (succ link_id) p
  | Rule (l, r) -> (link_id, [Either.Right (l, r)])

(* returns the free/local head link names *)	     
let collect_incidence (locals, frees) = function
  | LocalInd ((x, link_name), _) ->
     if List.mem x locals then failwith @@ "local link " ^ link_name ^ " is not univalent"
     else (x::locals, frees)
  | FreeInd (x, _) ->
     if List.mem x frees then failwith @@ "free link " ^ x ^ " is not univalent "
     else (locals, x::frees)

let collect_incidences = List.fold_left collect_incidence ([], [])
	    
let rec update fallback f x = function
  | [] -> fallback ()
  | (y, v) as h::t ->
     if x = y then (y, f v) :: t
     else h::update fallback f x t
		    
(* collect indeg and also check the serial condition*)
let rec collect_indeg_arg ((locals, frees) as links) = function
  | LocalLink (x, link_name) ->
     (update (fun _ -> failwith @@ link_name ^ " is not serial") succ x locals, frees)
  | FreeLink x -> 
     (locals, update (fun _ -> [x, 1]) succ x frees)
  | Atom' (p, xs) ->
     List.fold_left collect_indeg_arg links xs

let collect_indeg links = function
  | LocalInd (_, p) -> collect_indeg_arg links p
  | FreeInd  (_, p) -> collect_indeg_arg links p     

let collect_indegs = List.fold_left collect_indeg

let collect_link_info atoms =
  let (_, free_incidences) as links = collect_incidences atoms in
  let indegs =
    let zip_zero l = List.map (fun x -> (x, 0)) l in (* monomorphism restriction *)
    let init_indegs = first zip_zero @@ second zip_zero @@ links in
    collect_indegs init_indegs atoms
  in (indegs, free_incidences)

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

let check_rule (((_, lhs_links), lhs_rules), ((_, rhs_links), rhs_rules)) =
  if lhs_rules <> [] then failwith @@ "rule(s) on LHS"
  else if rhs_rules <> [] then failwith @@ "rule(s) on RHS is not supported ..."
  else check_link_cond (lhs_links, rhs_links)

let rec prep proc =
  let atoms, rules = partitionEithers @@ snd @@ prep_atoms [] 0 proc in 
  ((atoms, collect_link_info atoms), List.map prep_rule rules)
and prep_rule (lhs, rhs) =
  let rule = (prep lhs, prep rhs) in
  check_rule rule;
  Rule' rule
