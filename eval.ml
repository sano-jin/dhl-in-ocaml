(* eval.ml *)

open Syntax ;;

let (<.) f g = fun x -> f (g x)
let flip f x y = f y x  
  
(* Some helper functions for dealing the environment *)

let rec lookup var env =
  match env with
  | [] -> failwith @@ "unbound variable '" ^ var ^ "'"
  | (var2, refVal)::t ->
     if var = var2 then !refVal
     else lookup var t

let rec update var value env =
  match env with
  | [] -> failwith @@ "unbound variable '" ^ var ^ "'"
  | (var2, refVal)::t ->
     if var = var2 then refVal := value
     else update var value t

(* some helper functions *)		    
let extract_int = function
  | IntVal i -> i
  | _ -> failwith @@ "type error. expected int"
    
(* The evaluator *)
(* eval_exp : exp -> env -> int *)
let rec eval_exp env exp =
  let eval_binop f e1 e2 =
    f (eval_exp env e1) (eval_exp env e2)
  in  
  let eval_binop_int f =
    eval_binop (fun v1 v2 -> f (extract_int v1) (extract_int v2))
  in
  let value default = function
    | Some v -> v
    | None -> default
  in
  match exp with
  | Var var ->
     lookup var env
  | IntLit num -> IntVal num
  | BoolLit bool -> BoolVal bool
  | Plus (e1, e2) ->
     IntVal (eval_binop_int (+) e1 e2)
  | Times (e1, e2) ->
     IntVal (eval_binop_int ( * ) e1 e2)
  | Lt (e1, e2) ->
     BoolVal (eval_binop_int ( < ) e1 e2)
  | Func (args, body) ->
     FuncVal (args, body, env)
  | RecFunc (f, args, body) ->
     RecFuncVal (f, args, body, env)
  | App (f, args) ->
     let argVals = List.map (ref <. eval_exp env) args in
     match eval_exp env f with
     | FuncVal (vars, body, env') ->
	value VoidVal
	@@ flip eval_stmt body
	@@ List.combine vars argVals @ env'
     | RecFuncVal (f, vars, body, env') ->
	value VoidVal
	@@ flip eval_stmt body
	@@ (f, ref @@ RecFuncVal (f, vars, body, env'))::List.combine vars argVals @ env'
     | _ -> failwith @@ "expected function type"
  and eval_stmt env stmt =
    let mzero _ = None in
    match stmt with
    | Exp e ->
       mzero @@ eval_exp env e 
    | Assign (var, e) ->
       mzero @@ update var (eval_exp env e) env 
    | Seq (s1, s2) ->
       begin
       match eval_stmt env s1 with
       | None -> eval_stmt env s2
       | some -> some
       end
    | While (cond, stmt) ->
       begin
	match eval_exp env cond with
	| BoolVal true -> eval_stmt env @@ Seq (stmt, While (cond, stmt))
	| BoolVal false -> None
	| _ -> failwith @@ "expected boolean value"
       end
    | Let (var, e, stmt) ->
       eval_stmt ((var, ref @@ eval_exp env e)::env) stmt
    | Skip -> None
    | Return e -> Some (eval_exp env e)
    | Print e ->
       mzero
       @@ Printf.printf "%s\n"
       @@ string_of_value
       @@ eval_exp env e
