(* util.ml *)

let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let (>>=) = Option.bind
let (<$>) = Option.map
let (<::>) h t = List.cons h <$> t
let flip f x y = f y x  
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
let set_minus l r = List.filter (not <. flip List.mem r) l
let id x = x
let partitionEithers l = List.partition_map id l (* monomorphism restriction *)
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y
let rec uncurried_zip = function
  | ([], []) -> Some []
  | (xh::xt, yh::yt) -> (xh, yh) <::> uncurried_zip (xt, yt)
  | _ -> None

let rec zip t = curry uncurried_zip t
	   
let rec fold_maybe f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (fold_maybe f) t

let rec list_eq l r = match (l, r) with
  | ([], []) -> true
  | (xh::xt, yh::yt) -> xh = yh || list_eq xt yt
  | _ -> false
			     
let sym_diff l r =
  set_minus l r @ set_minus r l

let pair x y = (x, y)
let const x y = x
let list_singleton x = [x]
		  
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

