type exp =
  | Var of string
  | Abs of string * exp
  | App of exp * exp
  | Num of int
  | Bool of bool
  | Add of exp * exp
  | Sub of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Lt of exp * exp
  | Gt of exp * exp
  | Eq of exp * exp
  | If of exp * exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Float of float
  | Tuple of exp list
  | Proj of int * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Xor of exp * exp
  | String of string
  | Concat of exp * exp
and value =
  | VNum of int
  | VBool of bool
  | VClosure of string * exp * environment
  | VPair of value * value
  | VFloat of float
  | VTuple of value list
  | VString of string
  | VThunk of exp * environment
and environment = (string * value) list;;


let rec eval (expr: exp) (env: environment): value = match expr with
  | Var x ->
      (match List.assoc_opt x env with
       | Some (VThunk(e, thunk_env)) -> eval e thunk_env
       | Some other -> other
       | None -> failwith ("Unknown variable: " ^ x))
  
  | Abs(x, e) -> VClosure(x, e, env)
  
  | App(e1, e2) ->
      (match eval e1 env with
       | VClosure(x, body, closure_env) -> eval body ((x, VThunk(e2, env)) :: closure_env)
       | _ -> failwith "Application to a non-closure")
  
  | Num n -> VNum n
  | Bool b -> VBool b

  | Add(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VNum (n1 + n2)
  | Sub(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VNum (n1 - n2)
  | Mult(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VNum (n1 * n2)
  | Div(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VNum (n1 / n2)

  | Lt(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VBool (n1 < n2)
  | Gt(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VBool (n1 > n2)
  | Eq(e1, e2) -> 
      let VNum n1 = eval e1 env in let VNum n2 = eval e2 env in VBool (n1 = n2)

  | If(cond, e1, e2) -> 
      (match eval cond env with
       | VBool true -> eval e1 env
       | VBool false -> eval e2 env
       | _ -> failwith "Condition in IF is not a boolean")

  | Pair(e1, e2) -> VPair(eval e1 env, eval e2 env)
  | Fst(e) -> 
      (match eval e env with
       | VPair(v1, _) -> v1
       | _ -> failwith "First of a non-pair")
  | Snd(e) -> 
      (match eval e env with
       | VPair(_, v2) -> v2
       | _ -> failwith "Second of a non-pair")

  | Float f -> VFloat f
  | Tuple es -> VTuple (List.map (fun e -> eval e env) es)
  | Proj(index, e) -> 
      (match eval e env with
       | VTuple vs -> List.nth vs index
       | _ -> failwith "Projection from a non-tuple")

  | And(e1, e2) -> 
      let VBool b1 = eval e1 env in let VBool b2 = eval e2 env in VBool (b1 && b2)
  | Or(e1, e2) -> 
      let VBool b1 = eval e1 env in let VBool b2 = eval e2 env in VBool (b1 || b2)
  | Not(e) -> 
      let VBool b = eval e env in VBool (not b)
  | Xor(e1, e2) -> 
      let VBool b1 = eval e1 env in let VBool b2 = eval e2 env in VBool ((b1 || b2) && not (b1 && b2))

  | String s -> VString s
  | Concat(e1, e2) -> 
      let VString s1 = eval e1 env in let VString s2 = eval e2 env in VString (s1 ^ s2)
  | _ -> failwith "Expression type not supported in this implementation";; 
;;

(* This function initiates the evaluation of an expression, starting with an empty environment *)
let krivine_eval (exp: exp): value = eval exp [];;

(* Assuming the existence of 'krivine_eval' and necessary data types from your initial setup *)

(* Test cases setup *)
let tests = [
  (* Previously provided basic and complex test cases *)
  ("Simple Number", Num 5),
  ("Simple Boolean", Bool true),
  ("Addition", Add(Num 3, Num 7)),
  ("Subtraction", Sub(Num 10, Num 4)),
  ("Multiplication", Mult(Num 6, Num 7)),
  ("Division", Div(Num 8, Num 2)),
  ("Simple If-Else (True Condition)", If(Bool true, Num 1, Num 2)),
  ("Simple If-Else (False Condition)", If(Bool false, Num 1, Num 2)),
  ("Simple Function Application", App(Abs("x", Add(Var "x", Num 5)), Num 10)),
  ("Nested Function Application", App(Abs("x", App(Abs("y", Add(Var "x", Var "y")), Num 5)), Num 10)),
  ("Lazy Evaluation with If-Else", If(Bool true, Num 1, App(Abs("x", Var "x"), Var "x"))),
  ("Infinite Recursion Avoidance", App(Abs("x", If(Bool true, Num 42, App(Var "x", Var "x"))), Abs("y", App(Var "y", Var "y")))),
  ("Lazy Pair Evaluation", Fst(Pair(Num 1, App(Abs("x", Mult(Var "x", Num 1000)), Num 10000)))),
  ("Conditional Within Function", App(Abs("x", If(Eq(Var "x", Num 0), Abs("y", Add(Var "y", Num 1)), Abs("y", Sub(Var "y", Num 1)))), Num 0)),
  ("Lazy Tuple Projection", Proj(0, Tuple([Num 42; App(Abs("x", Add(Var "x", Num 100)), Num 10000)]))),

  (* New test cases highlighting benefits of lazy evaluation *)
  ("Lazy Undefined Avoidance", If(Bool true, Num 42, Var "undefined")),
  ("Lazy Infinite Loop with Conditional", If(Bool true, Num 42, App(Abs("x", Var "x"), Var "x"))),
  ("Lazy Function Not Evaluated", App(Abs("x", Num 42), If(Bool false, App(Abs("y", Var "y"), Var "y"), Num 5))),
  ("Lazy Pair Not Evaluated", Snd(Pair(Num 42, If(Bool false, App(Abs("x", Var "x"), Var "x"), Num 5)))),
  ("Deferred Computation", App(Abs("x", App(Var "x", Num 5)), Abs("y", Add(Var "y", Num 42)))),
  ("Lazy List Evaluation", Fst(Tuple([Num 1; If(Bool false, App(Abs("x", Var "x"), Var "x"), Num 2)]))),
  ("Selective Function Evaluation", App(Abs("x", If(Bool true, App(Var "x", Num 5), Num 0)), Abs("y", Num 42))),
  ("Deferred Recursive Computation", If(Bool true, Num 42, App(Abs("x", App(Var "x", Var "x")), Abs("y", Var "y")))),
  ("Nested Lazy Evaluation", If(Bool true, Num 42, App(Abs("x", If(Bool true, Num 42, Var "x")), Abs("y", Var "y")))),
  ("Lazy Function Application Inside Tuple", Proj(1, Tuple([Num 42; App(Abs("x", If(Bool true, Num 42, App(Var "x", Num 5))), Abs("y", Num 100))])))
];;


let rec string_of_value v = match v with
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure (x, _, _) -> Printf.sprintf "Closure(%s)" x
  | VPair (v1, v2) -> Printf.sprintf "Pair(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VFloat f -> string_of_float f
  | VTuple vs -> 
      let contents = vs |> List.map string_of_value |> String.concat ", " in
      Printf.sprintf "Tuple(%s)" contents
  | VString s -> Printf.sprintf "\"%s\"" s
  | VThunk (_, _) -> "Thunk"  (* You might want to expand this representation *)


(* Function to run and print results of test cases *)
let run_tests () =
  List.iteri (fun i (name, test_exp) ->
      let result = krivine_eval test_exp in
      Printf.printf "Test %d: %s\nResult: %s\n\n" (i + 1) name (string_of_value result)
    ) tests

(* Entry point to run tests *)
let () = run_tests () ;;
