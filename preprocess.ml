(* preprocess.ml *)

open Syntax
open Util

type local_link = int * string  (* (link_id, link_name_for_debugging) *)
		       
type p_arg =
  | PFreeLink of string
  | PLocalLink of local_link
  | PAtom of string * p_arg list

type p_atom =
  | PLocalInd of local_link * p_arg
  | PFreeInd of string * p_arg
			  
type p_atoms = p_atom list
type link_info = ((int * int) list * (string * int) list ) * string list
type p_rule = PRule of (((p_atoms * link_info) * p_rule list) * ((p_atoms * link_info) * p_rule list))

						      
let rec prep_arg env = function
  | Atom (p, xs) -> PAtom (p, List.map (prep_arg env) xs)
  | Link x -> match List.assoc_opt x env with
	      | None   -> PFreeLink x
	      | Some i -> PLocalLink (i, x)
			  
let rec prep_atoms env link_id = function
  | Zero -> (link_id, [])
  | Ind  (None, p) ->
     (succ link_id, [Either.Left (PLocalInd ((link_id, ""), prep_arg env p))])
  | Ind  (Some x, p) ->
     (link_id,
      [Either.Left (
	   match List.assoc_opt x env with
	   | None   -> PFreeInd (x, prep_arg env p)
	   | Some i -> PLocalInd ((i, x), prep_arg env p)
	 )])
  | Mol  (p, q) ->
     second List.concat @@ List.fold_left_map (prep_atoms env) link_id [p; q] 
  | New  (x, p) ->
     prep_atoms ((x, link_id)::env) (succ link_id) p
  | Rule (l, r) -> (link_id, [Either.Right (l, r)])

(* returns the free/local head link names *)	     
let collect_incidence (locals, frees) = function
  | PLocalInd ((x, link_name), _) ->
     if List.mem x locals then failwith @@ "local link " ^ link_name ^ " is not univalent"
     else (x::locals, frees)
  | PFreeInd (x, _) ->
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
  | PLocalLink (x, link_name) ->
     (update (fun _ -> failwith @@ link_name ^ " is not serial") succ x locals, frees)
  | PFreeLink x -> 
     (locals, update (fun _ -> [x, 1]) succ x frees)
  | PAtom (p, xs) ->
     List.fold_left collect_indeg_arg links xs

let collect_indeg links = function
  | PLocalInd (_, p) -> collect_indeg_arg links p
  | PFreeInd  (_, p) -> collect_indeg_arg links p     

let collect_indegs = List.fold_left collect_indeg

let collect_link_info atoms =
  let (_, free_incidences) as links = collect_incidences atoms in
  let indegs =
    let zip_zero l = List.map (fun x -> (x, 0)) l in (* monomorphism restriction *)
    let init_indegs = first zip_zero @@ second zip_zero @@ links in
    collect_indegs init_indegs atoms
  in (indegs, free_incidences)
