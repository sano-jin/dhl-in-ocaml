(* vm.ml *)

open Preprocess
open Util
       
type vm_atom =
  | VMAtom of string * node_ref list
  | VMInd of node_ref
 and node_ref = (int * vm_atom) ref  (* (indeg, atom) *)

let null_ptr: node_ref = ref (0, VMAtom ("Null", []))
    
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

let rec check_arg ((local_indegs, free_indegs) as indegs) env node_ref = function
  | FreeLink x ->
       begin
	 let indeg = List.assoc x free_indegs in
	 let update_free_addr2indeg fallback =
	   update_assc_opt (fun _ -> fallback) ((==) node_ref) (flip safe_minus indeg) env.free_addr2indeg
	 in
	 match List.assoc_opt x env.free2addr with 
	 | None -> (* if the free link name has not mathced to any address *)
	    if List.memq node_ref env.local_addrs then None (* Already matched with a local link *)
	    else 
	      (fun s -> { env with free2addr = (x, node_ref)::env.free2addr; free_addr2indeg = s})
	      <$> update_free_addr2indeg @@
		    (flip List.cons [] <. pair node_ref <$> safe_minus (fst !node_ref) indeg)
			       (*
		    let new_indeg = fst !node_ref - indeg in
		    if new_indeg < 0 then None
		    else Some [(node_ref, new_indeg)]
				*)
	 | Some addr ->   (* if the free link name has already mathced to the address *)
	    if addr != node_ref then None (* not univalent *)
	    else (fun s -> {env with free_addr2indeg = s})
		 <$> update_free_addr2indeg @@ failwith @@ "Bug: not found"
       end
  | LocalLink (x, _) ->
     begin
       match List.assoc_opt x env.local2addr with 
       | None -> if List.mem_assq node_ref env.free_addr2indeg then None (* already mathced with a free link *)
		 else if List.assoc x local_indegs <> fst !node_ref then None (* indeg did not match *)
		 else Some {env with local2addr = (x, node_ref)::env.local2addr; local_addrs = node_ref::env.local_addrs}
       | Some addr -> if node_ref != addr then None (* local link matched to different addrs *)
		      else Some env
     end	 
  | Atom' (p, xs) ->
     check_atom indegs env node_ref ((=) 1) (p, xs)
and check_atom indegs env node_ref indeg_pred (p, xs) =
  if List.memq node_ref env.local_addrs then None (* already matched addr  *)
  (* else if List.mem_assq node_ref env.free_addr2indeg then None (* already mathced with a free link *) *)
  else if not @@ indeg_pred @@ fst !node_ref then None (* indeg did not match *)
  else match snd !node_ref with
       | VMInd next ->
	  node_ref := !next; (* path compression *)
	  check_atom indegs env node_ref indeg_pred (p, xs)
       | VMAtom (q, ys) ->
	  if p <> q then None (* different atom name *)
	  else zip ys xs
	       >>= fold_maybe (uncurry <. check_arg indegs)
			      {env with local_addrs = node_ref::env.local_addrs} 					

			      
let rec check_ind ((local_indegs, free_indegs) as indegs) env node_ref = function
  | LocalInd ((x, _), Atom' (p, xs)) ->
     check_atom indegs {env with local2addr = insert x node_ref env.local2addr}
		     node_ref ((=) @@ List.assoc x local_indegs) (p, xs) 
  | FreeInd (x, Atom' (p, xs)) ->
     begin
       let indeg = List.assoc x free_indegs in
       let env = {env with free2addr = insert x node_ref env.free2addr} in
       update_assc_opt
	 (* has not mathced with the other free link *)
	 (fun _ -> flip List.cons [] <. pair node_ref <$> safe_minus (fst !node_ref) indeg)
	 ((==) node_ref)
	 (flip safe_minus indeg) env.free_addr2indeg
       >>= fun free_addr2indeg -> 
       check_atom indegs {env with free_addr2indeg = free_addr2indeg}
		  node_ref ((<=) 0) (p, xs)  (* the predicate ((<=) 0) should always hold *)
     end  
  | _ -> failwith @@ "Indirection on LHS is not supported"

let find_atom indegs env atom_list =
  let try_deref x link2addr ind =
    match List.assoc_opt x link2addr with
    | None -> one_of (flip (check_ind indegs env) ind) atom_list
    | Some node_ref -> check_ind indegs env node_ref ind
  in	 
  function
  | LocalInd ((x, _), _) as ind -> try_deref x env.local2addr ind 
  | FreeInd (x, _) as ind -> try_deref x env.free2addr ind 

				       

				       
