(** findatom.ml  *)

open Breakdown
open Util
open Vm

let cons_pair_ref node_ref indeg _ =
  let+ new_indeg = safe_minus (fst !node_ref) indeg in
  [(node_ref, new_indeg)]


let update_free_addr2indeg node_ref indeg env =
  let+ free_addr2indeg =
    update_assc_opt
      ((==) node_ref)
      (flip safe_minus indeg)
      (cons_pair_ref node_ref indeg)
      env.free_addr2indeg
  in
  {env with free_addr2indeg = free_addr2indeg}	   


let check_arg (local_indegs, free_indegs) env node_ref =
  function
  | BFreeLink x ->
       begin
	 let indeg = List.assoc x free_indegs in
	 match List.assoc_opt x env.free2addr with
	 | None -> 
	    (* if the free link name has not mathced to any address *)
	    if List.memq node_ref env.local_addrs then None (* Already matched with a local link  *)
	    else
	      let+ env = update_free_addr2indeg node_ref indeg env in
	      {env with free2addr = (x, node_ref)::env.free2addr}
	    
	 | Some addr ->
	    (* if the free link name has already mathced to the address  *)
	    if addr != node_ref then None (* has matched to other address  *)
	    else
	      update_free_addr2indeg node_ref indeg env
       end
  | BLocalLink x ->
     match List.assoc_opt x env.local2addr with 
     | None -> if List.mem_assq node_ref env.free_addr2indeg then None (* already mathced with a free link  *)
	       else if List.assoc x local_indegs <> fst !node_ref then None (* indeg did not match  *)
	       else Some {env with local2addr = (x, node_ref)::env.local2addr}
     | Some addr -> if node_ref != addr then None (* local link matched to different addrs  *)
		    else Some env


			      
let check_atom indegs indeg_pred (p, xs) env node_ref =
  let rec traverse node_ref =
    if List.memq node_ref env.local_addrs then None (* already matched addr   *)
    else if not @@ indeg_pred @@ fst !node_ref then None (* indeg did not match  *)
    else match snd !node_ref with
	 | VMInd next ->
	    
	    (* not working. must perform fusion  *)
	    (* node_ref := !next; (* path compression *) *)
	    
	    traverse next
		     
	 | VMAtom (q, ys) ->
	    if p <> q then None (* different atom name  *)
	    else zip ys xs
		 >>= foldM (uncurry <. check_arg indegs)
			   {env with local_addrs = node_ref::env.local_addrs}
  in traverse node_ref

	      
	      
let check_ind ((local_indegs, free_indegs) as indegs) env node_ref = function
  | BLocalInd (x, (p, xs)) ->
     check_atom
       indegs
       ((=) @@ List.assoc x local_indegs)
       (p, xs)

       (* if x is the key of `addr` in `env.local2addr`, then the `addr` should be equal to `node_ref`.
	  Since is the `x` is in the `env.local2addr`, then we should have conducted dereference hence
	  the `node_ref` is lookuped from the `env.local2addr` in the former phase (`try_deref` in `find_atoms`).        
	*)
       {env with local2addr = insert x node_ref env.local2addr}
       node_ref
  | BFreeInd (x, (p, xs)) ->
     let indeg = List.assoc x free_indegs in
     let* env = update_free_addr2indeg node_ref indeg env in
     check_atom
       indegs ((<=) 0) (p, xs)  (* the predicate ((<=) 0) should always hold  *)
       {env with
	 (* if x is the key of `addr` in `env.free2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.free2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.free2addr` in the former phase (`try_deref` in `find_atoms`).        
	  *)
	 free2addr = insert x node_ref env.free2addr;
       }
       node_ref 
  | _ -> failwith @@ "Indirection on LHS is not supported"
 

let rec find_atoms env redirs indegs atom_list =
  let check_ind_ = flip @@ check_ind indegs env in
  let try_deref x link2addr ind t =
    let* env = match List.assoc_opt x link2addr with
      | None -> one_of (check_ind_ ind) atom_list
      | Some node_ref -> check_ind_ ind node_ref
    in   find_atoms env redirs indegs atom_list t
  in	 
  function
  | BLocalInd (x, _) as ind ::t -> try_deref x env.local2addr ind t
  | BFreeInd  (x, _) as ind ::t -> try_deref x env.free2addr ind t
  | [] ->
     (* possibly checks the additional free redirection condition here  *)
     if redirs = () then Some env else None
  | _ -> failwith @@ "Indirection on LHS is not supported"


let find_atoms = find_atoms empty_env 
