(* findatom.ml *)

open Compile
open Util
open Vm
       
let check_arg (local_indegs, free_indegs) env node_ref = function
  | CFreeLink x ->
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
	 | Some addr ->   (* if the free link name has already mathced to the address *)
	    if addr != node_ref then None (* not univalent *)
	    else (fun s -> {env with free_addr2indeg = s})
		 <$> update_free_addr2indeg @@ failwith @@ "Bug: not found"
       end
  | CLocalLink x ->
     begin
       match List.assoc_opt x env.local2addr with 
       | None -> if List.mem_assq node_ref env.free_addr2indeg then None (* already mathced with a free link *)
		 else if List.assoc x local_indegs <> fst !node_ref then None (* indeg did not match *)
		 else Some {env with local2addr = (x, node_ref)::env.local2addr}
       | Some addr -> if node_ref != addr then None (* local link matched to different addrs *)
		      else Some env
     end

let rec check_atom indegs env node_ref indeg_pred (p, xs) =
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

			      
let check_ind ((local_indegs, free_indegs) as indegs) env node_ref = function
  | CLocalInd (x, (p, xs)) ->
     check_atom indegs {env with local2addr = insert x node_ref env.local2addr}
		node_ref ((=) @@ List.assoc x local_indegs) (p, xs) 
  | CFreeInd (x, (p, xs)) ->
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

		       (*
let find_atom indegs env atom_list =
  let try_deref x link2addr ind =
    match List.assoc_opt x link2addr with
    | None -> one_of (flip (check_ind indegs env) ind) atom_list
    | Some node_ref -> check_ind indegs env node_ref ind
  in	 
  function
  | CLocalInd (x, _) as ind -> try_deref x env.local2addr ind 
  | CFreeInd (x, _) as ind -> try_deref x env.free2addr ind 
  | _ -> failwith @@ "Indirection on LHS is not supported"

let find_atoms indegs atom_list atoms = foldM (flip (find_atom indegs) atom_list) empty_env atoms 
			*)

let rec find_atoms indegs atom_list env = 
  let try_deref x link2addr ind t =
    begin
      match List.assoc_opt x link2addr with
      | None ->
	 let find_others node_ref =
	   check_ind indegs env node_ref ind
	 in
	 one_of find_others atom_list
      | Some node_ref -> check_ind indegs env node_ref ind
    end >>= flip (find_atoms indegs atom_list) t
  in	 
  function
  | CLocalInd (x, _) as ind ::t -> try_deref x env.local2addr ind t
  | CFreeInd  (x, _) as ind ::t -> try_deref x env.free2addr ind t
  | [] -> Some env
  | _ -> failwith @@ "Indirection on LHS is not supported"

let find_atoms indegs atom_list atoms = find_atoms indegs atom_list empty_env atoms
  
		       (*
let find_atoms indegs atom_list atoms = foldM (find_atom indegs atom_list) empty_env atoms 
			*)
