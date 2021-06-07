(* preprocess.ml *)

open Syntax
  
let (<.) f g = fun x -> f (g x)
(*
  let (<$) f a = let _ = f a in a
 *)
let flip f x y = f y x  
let uncurry f x y = f (x, y)
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)

type local_link = int * string  (* (link_id, link_name_for_debugging) *)
		       
type arg' =
  | FreeLink of string
  | LocalLink of local_link
  | Atom' of string * arg' list

type atom =
  | LocalInd of local_link * arg'
  | FreeInd of string * arg'
			  
						      
let rec prep_arg env = function
  | Atom (p, xs) -> Atom' (p, List.map (prep_arg env) xs)
  | Link x -> match List.assoc_opt x env with
	      | None -> FreeLink x
	      | Some i -> LocalLink (i, x)
			  
let rec prep_atoms env link_id = function
  | Zero -> (link_id, [])
  | Ind  (None, p) ->
     (link_id + 1, [Either.Left (LocalInd ((link_id, ""), prep_arg env p))])
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
     prep_atoms ((x, link_id)::env) (link_id + 1) p
  | Rule (l, r) -> (link_id, [Either.Right (l, r)])

(* returns the free/local head link names *)	     
let collect_in_link_atom (locals, frees) = function
  | LocalInd ((x, link_name), _) ->
     if List.mem x locals then failwith @@ "local link " ^ link_name ^ " is not univalent"
     else (x::locals, frees)
  | FreeInd (x, _) ->
     if List.mem x frees then failwith @@ "free link " ^ x ^ " is not univalent "
     else (locals, x::frees)

let collect_in_link = List.fold_left collect_in_link_atom ([], [])
	    
let rec update fallback f x = function
  | [] -> fallback ()
  | (y, v) as h::t ->
     if x = y then (y, f v)::t
     else h::update fallback f x t

		    
(* collect indeg and also check the serial condition*)
let rec collect_indeg_arg ((locals, frees) as links) = function
  | LocalLink (x, link_name) ->
     (update (fun _ -> failwith @@ "local link " ^ link_name ^ " is not serial") succ x locals, frees)
  | FreeLink x -> 
     (locals, update (fun _ -> [x, 1]) succ x frees)
  | Atom' (p, xs) ->
     List.fold_left collect_indeg_arg links xs

let collect_indeg_atom links = function
  | LocalInd (_, p) -> collect_indeg_arg links p
  | FreeInd  (_, p) -> collect_indeg_arg links p     

let collect_indeg = List.fold_left collect_indeg_atom 

let collect_link_info atoms =
  let (_, free_incidences) as links = collect_in_link atoms in
  let indegs = 
    let map_pair_zero l = List.map (fun x -> (x, 0)) l in (* monomorphism restriction *)
    let init_indegs = first map_pair_zero @@ second map_pair_zero @@ links in
    collect_indeg init_indegs atoms
  in
  (indegs, free_incidences)

let set_minus l r = List.filter (not <. flip List.mem r) l

				
let check_link_cond ((lhs_atoms, (lhs_indegs, lhs_free_incidences)),
		     (rhs_atoms, (rhs_indegs, rhs_free_incidences))) =
  let _ =
    let free_names = List.map fst <. snd in
    let unbound_rhs_links = set_minus (free_names rhs_indegs) (free_names lhs_indegs) in
    if unbound_rhs_links <> [] then
      failwith @@ "link(s) " ^ String.concat ", " unbound_rhs_links ^ " on RHS has/have not appeared on LHS"
    else () in
  let _ =
    let unredired = set_minus lhs_free_incidences rhs_free_incidences in
    if unredired <> [] then 
      failwith @@ "link(s) " ^ String.concat ", " unredired ^ " on LHS is/are not redirected on RHS"
    else ()
  in ()

let check_rule ((lhs, lhs_rules), (rhs, rhs_rules)) =
  let string_of_rules =
    String.concat ", " <. List.map @@ fun (l,r) -> string_of_proc 0 @@ Rule (l, r) in
  let _ = 
    if lhs_rules <> [] then
      failwith @@ "rule(s) " ^ string_of_rules lhs_rules ^ " on LHS"
    else if rhs_rules <> [] then
      failwith @@ "rule(s) " ^ string_of_rules rhs_rules ^ " on RHS (rules on RHS are not supported)"
    else () in
  let _ = check_link_cond (lhs, rhs) in 
  ()    

let prep proc =
  let atoms, rules = List.partition_map (fun x -> x) @@ snd @@ prep_atoms [] 0 proc in 
  ((atoms, collect_link_info atoms), rules)
    
let prep_rule (lhs, rhs) =
  let rule = (prep lhs, prep rhs) in
  let _ = check_rule rule in
  rule

let preprocess = second @@ List.map prep_rule <. prep
  

    
