(* preprocess.ml *)

open Syntax
  
let (<.) f g = fun x -> f (g x)
let flip f x y = f y x  
let uncurry f x y = f (x, y)
		   
type arg' =
  | FreeLink of string
  | LocalLink of int
  | Atom' of string * arg' list

type atom =
  | LocalInd of int * arg'
  | FreeInd of string * arg'
			  
let second f (a, b) = (a, f b)
						      
let rec prep_arg env = function
  | Atom (p, xs) -> Atom' (p, List.map (prep_arg env) xs)
  | Link x -> match List.assoc_opt x env with
	      | None -> FreeLink x
	      | Some i -> LocalLink i
			  
let rec prep env link_id = function
  | Zero -> (link_id, [])
  | Ind  (None, p) ->
     (link_id + 1, [Either.Left (LocalInd (link_id, prep_arg env p))])
  | Ind  (Some x, p) ->
     (link_id,
      [Either.Left (
	   match List.assoc_opt x env with
	   | None -> FreeInd (x, prep_arg env p)
	   | Some i -> LocalInd (i, prep_arg env p)
	 )])
  | Mol  (p, q) ->
     second List.concat @@ List.fold_left_map (prep env) link_id [p; q] 
  | New  (x, p) ->
     prep ((x, link_id)::env) (link_id + 1) p
  | Rule (l, r) -> (link_id, [Either.Right (l, r)])

(* returns the free/local head link names *)	     
let check_univalent_atom (locals, frees) = function
  | LocalInd (x, _) ->
     if List.mem x locals then failwith @@ "local link appeared more than one"
     else (x::locals, frees)
  | FreeInd (x, _) ->
     if List.mem x frees then failwith @@ "free link " ^ x ^ " appeared more than one"
     else (locals, x::frees)

let check_univalent = List.fold_left check_univalent_atom ([], [])
	    
let rec update fallback f x = function
  | [] -> fallback
  | (y, v) as h::t ->
     if x = y then (y, f v)::t
     else h::update fallback f x t

(* collect indeg and also check the serial condition*)
let rec collect_indeg_arg ((locals, frees) as links) = function
  | LocalLink x ->
     (update (failwith @@ "not serial") succ x locals, frees)
  | FreeLink x -> 
     (locals, update ([x, 1]) succ x frees)
  | Atom' (p, xs) ->
     List.fold_left collect_indeg_arg links xs

let collect_indeg_atom links = function
  | LocalInd (_, p) -> collect_indeg_arg links p
  | FreeInd (_, p) -> collect_indeg_arg links p     

let collect_indeg = List.fold_left collect_indeg_atom 



				   
(*
		    
let check_serial locals atoms =
  List.for_all (check_serial_arg locals) atoms
 *)		     

	       


				   (*
type link_info = {
  link_id: int option; (* Some local_link_id | None *)
  indeg: int;
  has_incidence: bool; (* has_head *)
}
type free_link_env = (string * link_info) list  (* (link_name, link_info) list *)
type local_link_env = (int * int) list          (* (link_id, indeg) *)
				    *)				     



				   (*
type envs = {
   free_links: free_link_env;
   local_links: local_link_env;
   link_id: int; (* the seed/number of the local links that has distributed an id *)
}
    			  
type rule_set = (proc' * proc') list  (* (lhs, rhs) list *)
 and proc' = {
   atoms: atom list;
   rules: rule_set;
   envs: envs;
 }

				    *)

