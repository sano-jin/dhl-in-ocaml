(* pushatom.ml *)

open Compile
open Util
open Vm
       
let push_arg (local2addr, free2addr) =
  let get_link x link2addr = 
    match List.assoc_opt x link2addr with
    | None ->
       let node_ref = ref (0, VMAtom ("Null", [])) in
       ((x, node_ref)::link2addr, node_ref)
    | Some node_ref -> (link2addr, node_ref)
  in
  function
  | CFreeLink x ->
     first (pair local2addr) @@ get_link x free2addr
  | CLocalLink x ->
     first (flip pair free2addr) @@ get_link x local2addr

let push_atom (local_indegs, free_indegs) link2addrs =
  let get_atom (x, p_xs) indegs link2addr = 
    let atom = (List.assoc x indegs, p_xs) in
    match List.assoc_opt x link2addr with
    | None -> (x, ref atom)::link2addr
    | Some node_ref ->
       node_ref := atom;
       link2addr
  in
  let get_links = List.fold_left_map push_arg link2addrs in
  function
  | CFreeInd (x, (p, xs)) ->
     let ((local2addr, free2addr), xs) = get_links xs in
     (local2addr, get_atom (x, VMAtom (p, xs)) free_indegs free2addr)
  | CLocalInd (x, (p, xs)) ->
     let ((local2addr, free2addr), xs) = get_links xs in
     (get_atom (x, VMAtom (p, xs)) local_indegs local2addr, free2addr)
  | CRedir (x, y) ->
     if List.assoc x free_indegs = 0 then link2addrs
     else	  
       let ((local2addr, free2addr), y) = push_arg link2addrs @@ CFreeLink y in
       (local2addr, get_atom (x, VMInd y) free_indegs free2addr)

let push_atoms (local_indegs, free_indegs) free_addr2indeg free2addrs inds =
  let free_indegs =
    flip List.map free_indegs
    @@ fun (x, indeg) ->
       (x, List.assoc (List.assoc x free2addrs) free_addr2indeg + indeg)
  in
  List.map snd @@ fst
  @@ List.fold_left (push_atom (local_indegs, free_indegs)) ([], free2addrs) inds
		    
