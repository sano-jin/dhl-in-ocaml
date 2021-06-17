(** findatom.ml  *)

open Breakdown
open Util
open Vm

let check_arg local_indegs env node_ref =
  function
  | BFreeLink x ->
       begin
	 match List.assoc_opt x env.free2addr with
	 | None -> 
	    (* if the free link name has not mathced to any address *)
	    
	    if List.memq node_ref @@ List.map snd env.local2addr then None (* Already matched with a local link  *)
	    else
	      Some {env with free2addr = (x, node_ref)::env.free2addr}
	    
	 | Some addr ->
	    (* if the free link name has already mathced to the address  *)
	    if addr != node_ref then None (* has matched to other address  *)
	    else Some env
       end
  | BLocalLink x ->
     match List.assoc_opt x env.local2addr with 
     | None -> if List.memq node_ref @@ List.map snd env.free2addr then None (* already mathced with a free link  *)
	       else if List.assoc x local_indegs <> fst !node_ref then None (* indeg did not match  *)
	       else Some {env with local2addr = (x, node_ref)::env.local2addr}
     | Some addr -> if node_ref != addr then None (* local link matched to different addrs  *)
		    else Some env


			      
let check_atom local_indegs indeg_pred (p, xs) env node_ref =
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
		 >>= foldM (uncurry <. check_arg local_indegs)
			   {env with local_addrs = node_ref::env.local_addrs}
  in traverse node_ref

	      
	      
let check_ind local_indegs env node_ref = function
  | BLocalInd (x, (p, xs)) ->
     check_atom
       local_indegs
       ((=) @@ List.assoc x local_indegs)
       (p, xs)

       (* if x is the key of `addr` in `env.local2addr`, then the `addr` should be equal to `node_ref`.
	  Since is the `x` is in the `env.local2addr`, then we should have conducted dereference hence
	  the `node_ref` is lookuped from the `env.local2addr` in the former phase (`try_deref` in `find_atoms`).        
	*)
       {env with local2addr = insert x node_ref env.local2addr}
       node_ref
  | BFreeInd (x, (p, xs)) ->
     check_atom
       local_indegs (const true) (p, xs)  (* no indeg checking for a free link *)
       {env with
	 (* if x is the key of `addr` in `env.free2addr`, then the `addr` should be equal to `node_ref`.
	    Since is the `x` is in the `env.free2addr`, then we should have conducted dereference hence
	    the `node_ref` is lookuped from the `env.free2addr` in the former phase (`try_deref` in `find_atoms`).        
	  *)
	 free2addr = insert x node_ref env.free2addr;
       }
       node_ref 
  | _ -> failwith @@ "Indirection on LHS is not supported"
 

let rec find_atoms env redirs ((local_indegs, free_indegs) as indegs) atom_list =
  let check_ind_ = flip @@ check_ind local_indegs env in
  let try_deref x link2addr ind t =
    match List.assoc_opt x link2addr with
    | None ->
       let rec try_match = function
	 | [] -> None (* no atom list remaining *)
	 | node_ref::node_refs ->
	    ( let* env = check_ind_ ind node_ref in
	      find_atoms env redirs indegs atom_list t
	    ) <|> fun _ -> try_match node_refs
       in try_match atom_list
    | Some node_ref ->
       let* env = check_ind_ ind node_ref in
       find_atoms env redirs indegs atom_list t
  in	 
  function
  | BLocalInd (x, _) as ind ::t -> try_deref x env.local2addr ind t
  | BFreeInd  (x, _) as ind ::t -> try_deref x env.free2addr ind t
  | [] ->
     (* calculate free_addr2indeg *)
     let free_addr2indeg =
       let free_addr2before_indeg =
	 List.map (fun (_, node_ref) -> (node_ref, fst !node_ref)) env.free2addr
       in
       List.fold_left (fun free_addr2before_indeg (link_name, indeg) ->
		 let node_ref = List.assoc link_name env.free2addr in
		 update (fun _ -> failwith "Bug") ((-) indeg) node_ref free_addr2before_indeg)
		free_addr2before_indeg
		free_indegs
     in
     let env = {env with free_addr2indeg = free_addr2indeg} in
	   (* possibly checks the additional free redirection condition here  *)
     if redirs = () then Some env else None
  | _ -> failwith @@ "Indirection on LHS is not supported"


let find_atoms redirs indegs atom_list = find_atoms empty_env redirs indegs atom_list
