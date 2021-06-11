(* util.ml *)

let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let (<...) f g = fun x y z -> f (g x y z)
let (>>=) = Option.bind
let (<$>) = Option.map
let (<::>) h t = List.cons h <$> t
let flip f x y = f y x  
let (<&>) x f = Option.map f x
let rec foldM f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (foldM f) t
			     
let second f (a, b) = (a, f b)
let first f (a, b) = (f a, b)
let swap (a, b) = (b, a)
let rot_left (a, (b, c)) = ((a, b), c)
let rot_right ((a, b), c) = (a, (b, c))
let set_minus l r = List.filter (not <. flip List.mem r) l
let set_minus_q l r = List.filter (not <. flip List.memq r) l
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

let (<|>) l f = 
  if Option.is_some l then l
  else f ()

let rec one_of f = function
  | [] -> None
  | h::t -> f h <|> fun _ -> one_of f t

let rec insert x v = function
  | [] -> [(x, v)]
  | (y, w) as h ::t ->
     if x = y then
       if v <> w then failwith @@ "Bug: updating"
       else (x, v)::t
     else h::insert x v t

let rec update_assc_opt fallback pred f = function
  | [] -> fallback ()
  | (y, v) as h ::t ->
     if pred y then flip List.cons t <. pair y <$> f v
     else h <::> update_assc_opt fallback pred f t

let safe_minus x y =
  if x < y then None else Some (x - y)


let maybe default = function
  | None -> default
  | Some s -> s
		    

let (<~>) h t =
  maybe t @@ (flip List.cons t <$> h)
		  
				    

(* let list_singleton x = [x] *)
		  
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

