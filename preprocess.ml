(* preprocess.ml *)

open Syntax ;;

let (<.) f g = fun x -> f (g x)
let flip f x y = f y x  

type link_info = {
  link_id: int option; (* Some local_link_id | None *)
  indeg: int;
  has_incidence: bool; (* has_head *)
}
type free_link_env = (string * link_info) list  (* (link_name, link_info) list *)
type local_link_env = (int * int) list          (* (link_id, indeg) *)
				     
type arg' =
  | FreeLink of string
  | LocalLink of int
  | Atom' of string * arg' list

type atom =
  | LocalInd of int * arg'
  | FreeInd of string * arg'

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

let second f (a, b) = (a, f b)

let rec lookup_in_link_info x = function
  | [] ->
     let link_info = {link_id = None; indeg = 0; has_incidence = true}
     in  (link_info, [(x, link_info)])
  | (y, link_info) as h ::t ->
     if x = y then
       let link_info' =
	 if link_info.has_incidence then failwith @@ "link " ^ x ^ " already has an incidence" 
	 else {link_info with has_incidence = true}
       in  (link_info', (y, link_info')::t)
     else second (List.cons h) @@ lookup_in_link_info x t 

			
let prep_arg envs p = (envs, Atom' ("a", []))
						      
let rec prep acc = function
  | Zero -> acc
  | Ind  (None, p) ->
     let envs = {acc.envs with 
		  local_links = (acc.envs.link_id, 0)::acc.envs.local_links;
		  link_id = acc.envs.link_id + 1}
     in let (envs, p) = prep_arg envs p
	in  {acc with atoms = LocalInd (acc.envs.link_id, p)::acc.atoms; envs = envs}
  | Ind  (Some x, p) ->
     let (link_info, free_links) = lookup_in_link_info x acc.envs.free_links
     in  {acc with envs = {acc.envs with free_links = free_links}}
  | Mol  (p, q) -> acc
  | New  (x, p) -> acc
  | Rule (l, r) -> acc
	       

	       
