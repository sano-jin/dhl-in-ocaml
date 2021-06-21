(* heap.ml *)

open Util

type 'a memory_entity =
  | InvalidMem (* discarded memory unit *)
  | ValidMem of 'a
 and 'a pointer = int * 'a memory_entity ref		 

let is_valid_pointer p = match !(snd p) with
  | InvalidMem -> false
  | ValidMem _ -> true
			   
let free_addr = ref 0
let allocated_memory = ref ([]: int list)
let discarded_memory = ref ([]: int list)
		    
let addr_of_pointer = fst
let string_of_pointer p = "#" ^ string_of_int @@ addr_of_pointer p

let ( !^ ) p = match !(snd p) with
  | InvalidMem -> invalid_arg @@ "Segfault: invalid access to " ^ string_of_pointer p
  | ValidMem r -> !r;;

let ( ^:= ) p v = match !(snd p) with
  | InvalidMem -> invalid_arg @@ "Segfault: invalid update at " ^ string_of_pointer p
  | ValidMem r -> r := v;;			

let new_pointer x =
  let pointer = (!++ free_addr, ValidMem (ref x)) in
  allocated_memory := addr_of_pointer pointer::!allocated_memory;
  pointer

let delete_pointer p =
  let addr = addr_of_pointer p in
  if List.mem addr !discarded_memory
  then failwith @@ "Double free at " ^ string_of_pointer p
  else
    discarded_memory := addr_of_pointer p::!discarded_memory

let string_of_memory string_of_a p =
  string_of_pointer p ^ ": " ^
    match !(snd p) with
    | InvalidMem -> "InvalidMem"
    | ValidMem a -> string_of_a a

				
