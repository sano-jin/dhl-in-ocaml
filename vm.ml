(* vm.ml *)

open Preprocess
open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let null: node_ref = ref (0, VMAtom ("Null", [])) 

(* atom list *)
type atom_list = node_ref list

(* atom register *)
type atom_reg = node_ref list

(* indeg register *)
type indeg_reg = (int * int) list     (* (address, indeg) list *)

type env = {
    (* A set of the addresses which local links have matched.
     * Containes the addresses to the "embedded atom"s. *)
    local_addrs: node_ref list;

    (* A map from the id of matched local links to the matched addresses
     * This does not contain the addresses to the "embedded atom"s on the LHS.
     * Since the local links of them are not known. *)
    local2addr: (int * node_ref) list;

    (* A map from free link names on the LHS to the matched addresses on the heap.  *)
    free2addr: (string * node_ref) list;

    (* A map from the addresses on the heap which the free links have matched
     * to the indegrees that are left.
     * That is, matching free links will consume the indegrees in this map
     * Notice the indegrees should be kept as non-negative value. *)
    free_addr2indeg: (node_ref * int) list;

}

let rec update_assoc fallback pred f = function
  | [] -> fallback
  | (y, v) as h::t ->
     if pred y then (fun s -> (y, s)::t) <$> f v
     else h <::> update_assoc fallback pred f t

let decr_indeg diff old =
  if old < diff then None else Some (old - diff)

let rec check_arg (locals, frees) env node_ref = function
  | FreeLink x ->
     begin
       let indeg = List.assoc x frees in
       let update_free_addr2indeg fallback =
	 update_assoc fallback ((==) node_ref) (decr_indeg indeg) env.free_addr2indeg
       in
       match List.assoc_opt x env.free2addr with 
       | None -> (* if the free link name has not mathced to any address *)
	  (fun s -> { env with free2addr = (x, node_ref)::env.free2addr; free_addr2indeg = s})
	  <$> update_free_addr2indeg @@
	    let new_indeg = fst !node_ref - indeg in
	    if new_indeg < 0 then None
	    else Some [(node_ref, new_indeg)]
       | Some addr ->   (* if the free link name has already mathced to the address *)
	  if addr != node_ref then None (* not univalent *)
	  else (fun s -> {env with free_addr2indeg = s})
	       <$> update_free_addr2indeg @@ failwith @@ "not found"
     end
  | LocalLink (x, _) ->
     begin
       match List.assoc_opt x env.local2addr with 
       | None -> if List.assoc x locals <> fst !node_ref then None (* indeg did not match *)
		 else Some {env with local2addr = (x, node_ref)::env.local2addr; local_addrs = node_ref::env.local_addrs}
       | Some addr -> if node_ref != addr then None (* local link matched to different addrs *)
		      else Some env
     end	 
  | Atom' (p, xs) as atom ->
     if List.memq node_ref env.local_addrs then None (* already matched addr  *)
     else if fst !node_ref <> 1 then None (* indeg did not match *)
     else match snd !node_ref with
	  | VMInd next -> (* node_ref := !next; (* path compression *) *)
	     check_arg (locals, frees) env node_ref atom
	  | VMAtom (q, ys) ->
	     if p <> q then None (* different atom name *)
	     else curried_zip ys xs >>= fold_maybe (uncurry <. check_arg (locals, frees)) env 
     
