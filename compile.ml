(* compile.ml *)

open Preprocess

(* instruction *)
type inst =
  | FindAtom of string * int * int   (* FindAtom (atom_name, arity) *)
  | Neq of int * int                 (* Neq (atom_reg_i_1, atom_reg_i_2) *)
  | Eq of int * int                  (* Eq (atom_reg_i_1, atom_reg_i_2) *)
  | Deref of int * int               (* Deref (atom_reg_i, link_i) *)
  | IndegLe of int * int             (* IndegLe (atom_reg_i, indeg) *)
  | IndegEq of int * int             (* IndegEq (atom_reg_i, indeg) *)
  | Func of int * string * int       (* Func (atom_reg_i, atom_name, arity) *)
        
      
