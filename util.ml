(* util.ml *)

let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let flip f x y = f y x  
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
let set_minus l r = List.filter (not <. flip List.mem r) l
let id x = x
let partitionEithers l = List.partition_map id l (* monomorphism restriction *)

(* 
let (<$) f a = let _ = f a in a
let uncurry f x y = f (x, y)
 *)


(* read lines from the given file *)
let read_file name : string =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
       close_in ic;
       String.concat "\n" @@ List.rev @@ "" :: acc
  in
  loop []

