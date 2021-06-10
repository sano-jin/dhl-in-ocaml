(* compile.ml *)

open Preprocess

type c_arg =
  | CFreeLink of string
  | CLocalLink of int

type c_atom = string * c_arg list

type c_ind =
  | CLocalInd of int * c_atom
  | CFreeInd of string * c_atom
  | CRedir of string * string

			 
