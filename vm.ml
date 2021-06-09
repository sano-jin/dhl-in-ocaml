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

type env =
    (int * node_ref) list
    (* the map from the id of matched local links
     * to the matched addresses
     *)
    * ((string * int) list) (* the map from the addresses of the free links to the *)
	     
