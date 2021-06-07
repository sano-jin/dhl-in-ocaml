(* preprocess.ml *)

open Syntax
  
let (<.) f g = fun x -> f (g x)
let flip f x y = f y x  
let uncurry f x y = f (x, y)
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
			
type arg' =
  | FreeLink of string
  | LocalLink of int
  | Atom' of string * arg' list

type atom =
  | LocalInd of int * arg'
  | FreeInd of string * arg'
			  
						      
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
let collect_in_link_atom (locals, frees) = function
  | LocalInd (x, _) ->
     if List.mem x locals then failwith @@ "local link appeared more than one"
     else (x::locals, frees)
  | FreeInd (x, _) ->
     if List.mem x frees then failwith @@ "free link " ^ x ^ " appeared more than one"
     else (locals, x::frees)

let collect_in_link = List.fold_left collect_in_link_atom ([], [])
	    
let rec update fallback f x = function
  | [] -> fallback x
  | (y, v) as h::t ->
     if x = y then (y, f v)::t
     else h::update fallback f x t

(* collect indeg and also check the serial condition*)
let rec collect_indeg_arg ((locals, frees) as links) = function
  | LocalLink x ->
     (update (fun _ ->
	  failwith
	  @@ string_of_int x ^ " not serial in" ^
	       "[" ^ String.concat "; " (List.map (fun (x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")") locals) ^ "]")
	     succ x locals, frees)
  | FreeLink x -> 
     (locals, update (fun _ -> [x, 1]) succ x frees)
  | Atom' (p, xs) ->
     List.fold_left collect_indeg_arg links xs

let collect_indeg_atom links = function
  | LocalInd (_, p) -> collect_indeg_arg links p
  | FreeInd (_, p) -> collect_indeg_arg links p     

let collect_indeg = List.fold_left collect_indeg_atom 

let collect_link_info proc =
  let atoms, rules = List.partition_map (fun x -> x) @@ snd @@ prep [] 0 proc in 
  let map_pair_zero l = List.map (fun x -> (x, 0)) l in (* monomorphism restriction *)
  let links = first map_pair_zero @@ second map_pair_zero @@ collect_in_link atoms in
  let link_info = collect_indeg links atoms in
  (atoms, link_info, rules)

