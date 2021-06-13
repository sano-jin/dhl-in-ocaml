(* util.ml *)

let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
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
let zip t = curry uncurried_zip t
let sym_diff l r =
  set_minus l r @ set_minus r l

let pair x y = (x, y)
let const x _ = x

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

let update_ref f r = r := f !r

let safe_minus x y =
  if x < y then None else Some (x - y)

let maybe default = function
  | None -> default
  | Some s -> s 
