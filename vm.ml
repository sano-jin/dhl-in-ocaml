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
     if pred y then Option.map (fun s -> (y, s)::t) @@ f v
     else Option.map (List.cons h) @@ update_assoc fallback pred f t

let decr_indeg diff old =
  if old < diff then None else Some (old - diff)

let checkArg node_ref (locals, frees) env = function
  | FreeLink x ->
     begin
       let indeg = List.assoc x frees in
       match List.assoc_opt x env.free2addr with 
       | None -> (* if the free link name has not mathced to any address *)
	  begin
	  match update_assoc
		  (let new_indeg = fst !node_ref - indeg in
		   if new_indeg < 0 then None
		   else Some [(node_ref, new_indeg)]
		  )
		  ((==) node_ref)
		  (decr_indeg indeg)
		  env.free_addr2indeg
	  with
	  | None -> None
	  | Some s -> Some { env with
			     free2addr = (x, node_ref)::env.free2addr;
			     free_addr2indeg = s
			   }
	  end
       | Some s -> None
     end
  | LocalLink x -> None
  | Atom' (p, xs) -> None
