(* syntax.ml *)

let value default = function
  | None -> default
  | Some s -> s		      
		      
type arg =
  | Atom of string * arg list  (* atom. e.g. a(X, Y) *)
  | Link of string             (* link. e.g. X *)

(* process *)
type proc = 
  | Rule of proc * proc         (* rule. e.g. a :- b. *)
  | Ind of string option * arg  (* indirection. e.g. X -> a(Y) *)
  | Mol of proc * proc          (* molecule. e.g. (P, Q) *)  
  | Zero
  | New of string * proc

let rec string_of_arg = function
  | Link x -> x
  | Atom (a, xs) ->
     if xs = [] then a 
     else a ^ "(" ^ String.concat ", " (List.map string_of_arg xs) ^ ")"							       		      
let rec string_of_proc priority = function
  | Zero -> ""
  | Ind (from, _to) ->
     let str_of_from = match from with
       | None -> ""
       | Some s -> s ^ " -> "
     in str_of_from ^ string_of_arg _to
  | Rule (lhs, rhs) ->
     let str_of_rule =
       string_of_proc 1 lhs ^ " :- " ^ string_of_proc 1 lhs
     in
     if priority < 1 then "(" ^ str_of_rule ^ ")"
     else str_of_rule
  | Mol (p, q) ->
     let str_of_mol sep =
       string_of_proc 2 p ^ sep ^ string_of_proc 2 q
     in
     if priority = 0 then str_of_mol "." ^ "."
     else if priority < 2 then "(" ^ str_of_mol "," ^ ")" 
     else str_of_mol ","
  | New (x, proc) ->
     let str_of_new = 
       "\\" ^ x ^ "." ^ string_of_proc 3 proc
     in
     if priority < 3 then "(" ^ str_of_new ^ ")"
     else str_of_new
     
