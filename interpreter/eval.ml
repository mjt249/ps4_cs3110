open Ast

type builtin = value list -> environment -> value

and procedure =
  | ProcBuiltin of builtin
  | ProcLambda of variable list * environment * expression list

and value =
  | ValDatum of datum
  | ValProcedure of procedure

and binding = value ref Environment.binding
and environment = value ref Environment.environment


(* Parses a datum into an expression. *)
let rec read_expression (input : datum) : expression =
   let rec cons_to_expr_list (cons: datum) (acc: expression list) : expression list =
    match cons with
    | Cons ( first, second ) -> cons_to_expr_list second ((read_expression first)::acc) 
    | Nil -> List.rev(acc)
    | _ -> failwith "not a cons" in 

  match input with
  | Atom (Identifier id) when Identifier.is_valid_variable id ->

     ExprVariable (Identifier.variable_of_identifier id)
  | Atom (Identifier id) -> failwith "is a keyword"
  | Atom (Boolean bl) -> ExprSelfEvaluating (SEBoolean bl)
  | Atom (Integer intgr) -> ExprSelfEvaluating (SEInteger intgr)

  | Cons ( Atom (Identifier id), in_quote) when (id = (Identifier.identifier_of_string "quote")) -> 
        ExprQuote in_quote
  | Cons ( Atom (Identifier id), 
    Cons (e1, (Cons (e2, Cons (e3, Nil))))) when (id = (Identifier.identifier_of_string "if")) ->
        ExprIf (read_expression e1, read_expression e2, read_expression e3)
  | Cons ( Atom (Identifier id), the_rest) when Identifier.is_valid_variable id->
        ExprProcCall ( ExprVariable (Identifier.variable_of_identifier id), cons_to_expr_list the_rest [] ) 

       (*| Atom (Identifier id) -> when id = "lambda" -> failwith "lambda"
       | Atom (Identifier id) -> when id = "set!" -> failwith "set!"
       | Atom (Identifier id) -> when id = "let" -> failwith "let"
       | Atom (Identifier id) -> when id = "let*" -> failwith "let*"
       | Atom (Identifier id) -> when id = "letrec" -> failwith "letrec"
       | Atom (Identifier id) when Identifier.is_valid_variable id ->
          ExprVariable id *)
  | _ -> failwith "read failed"
     (* Above match case didn't succeed, so id is not a valid variable. *)
 (* | _ -> failwith "Everything you do is just amazing!"*)


(* Parses a datum into a toplevel input. toplevel = definition | expression. 
   so call read_expression and then if it fails, try definition. if that fails
   parsing failed.*) 
let read_toplevel (input : datum) : toplevel =
  match input with
  | _ -> failwith "Sing the Rowing Song!"

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =

  (*single cell is a value list with exactly one cons cell*)
  let func_car (single_cell: value list) (env: environment): value = 

    match single_cell with
    | (ValDatum (Cons (d1, d2)))::[] -> ValDatum d1
    | _ -> failwith "Invalid arguments to car." in
  (*single cell is a value list with exactly one cons cell*)
  let func_cdr (single_cell: value list) (env: environment): value=
    match single_cell with
    | (ValDatum (Cons (d1, d2)))::[] -> ValDatum d2
    | _ -> failwith "Invalid arguments to cdr." in
  let func_add (ints: value list) (env: environment): value =
    let rec add_helper (numbers: value list) (acc:int) =
      match numbers with
      | (ValDatum (Atom (Integer i)))::[]-> ValDatum (Atom (Integer (acc + i)))
      | (ValDatum (Atom (Integer i)))::tl-> add_helper tl (acc + i)
      | _ -> failwith "Invalid arguments to +." in
    add_helper ints 0 in

  let func_mult (ints: value list) (env: environment) : value =
    let rec mult_helper (numbers: value list) (acc: int) =
      match numbers with
      | (ValDatum (Atom (Integer i)))::[]-> ValDatum (Atom (Integer (acc * i)))
      | (ValDatum (Atom (Integer i)))::tl-> mult_helper tl (acc * i)
      | _ -> failwith "Invalid arguments to *." in
    mult_helper ints 1 in

  let func_eq (two_vals: value list) (env: environment) : value =
    match two_vals with
    | e1::e2::[] -> ValDatum (Atom (Boolean (e1 = e2)))
    | _ -> failwith "Invalid arguments to eqaul?." in

  let func_eval (one_val: value list) (env: environment) : value =
    match one_val with
    | (ValDatum one_datum)::[] -> eval (read_expression one_datum) env
    | _ -> failwith "Invalid arguments to eval." in
  
  let init_env = Environment.empty_environment in

  let course_ref = ref (ValDatum (Atom (Integer 3110))) in
  let course_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("course")) in
  let course_env = Environment.add_binding init_env (course_var, course_ref) in

  let car_ref = ref (ValProcedure (ProcBuiltin func_car)) in
  let car_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("car")) in
  let car_env = Environment.add_binding course_env (car_var, car_ref) in

  let cdr_ref = ref (ValProcedure (ProcBuiltin func_cdr)) in
  let cdr_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("cdr")) in
  let cdr_env = Environment.add_binding car_env (cdr_var, cdr_ref) in

  let add_ref = ref (ValProcedure (ProcBuiltin (func_add))) in
  let add_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("+")) in
  let add_env =  Environment.add_binding cdr_env (add_var, add_ref) in

  let mult_ref = ref (ValProcedure (ProcBuiltin (func_mult))) in
  let mult_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("*")) in
  let mult_env =  Environment.add_binding add_env (mult_var, mult_ref) in

  let eq_ref = ref (ValProcedure (ProcBuiltin (func_eq))) in
  let eq_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("equal?")) in
  let eq_env =  Environment.add_binding mult_env (eq_var, eq_ref) in

  let eval_ref = ref (ValProcedure (ProcBuiltin (func_eval))) in
  let eval_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("eval")) in
  Environment.add_binding eq_env (eval_var, eval_ref) 






(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  let rec expr_list_to_val_list (e_list: expression list) (acc: value list): value list =
    match e_list with 
    | hd::tl -> expr_list_to_val_list tl ((eval hd env)::acc)
    | _ -> List.rev(acc) in

  let variable_eval (var_expr: variable) (env: environment) : value =
    !(Environment.get_binding env var_expr) in

  let self_eval_eval (se_expr: self_evaluating) : value =
    match se_expr with
    | SEInteger integer -> ValDatum (Atom (Integer integer))
    | SEBoolean boolean -> ValDatum (Atom (Boolean boolean)) in

  let quote_eval (quote_expr : datum) : value =
    match quote_expr with
    | Cons (first, second) -> ValDatum first
    | _ -> failwith "quote_eval shouldn't be coming here..." in

  let if_eval (e1: expression) (e2: expression) (e3: expression) (env: environment) : value =
    if (e1 = (ExprSelfEvaluating (SEBoolean false))) then (eval e3 env) else (eval e2 env) in

  match expression with

  | ExprSelfEvaluating se -> self_eval_eval se
  | ExprVariable variable -> variable_eval variable env
  | ExprQuote datum       -> quote_eval datum
  | ExprLambda (_, _) -> failwith "lambda"
  | ExprProcCall (ExprVariable id, e_list) -> 
(*   and procedure =
  | ProcLambda of variable list * environment * expression list
  value =
  | ValProcedure of procedure
 *)
      (match (!(Environment.get_binding env id)) with
       | ValProcedure (ProcBuiltin builtin) -> builtin (expr_list_to_val_list e_list []) env
       | _ -> failwith "not a valid proc call")
  | ExprIf (e1, e2, e3) -> if_eval e1 e2 e3 env
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."
  | _ -> failwith "no match"

(* Evaluates a toplevel input down to a value and an output environment in a
   given environment. *)
let eval_toplevel (toplevel : toplevel) (env : environment) :
      value * environment =
  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (_, _)     ->
     failwith "I couldn't have done it without the Rower!"

let rec string_of_value value =
  let rec string_of_datum datum =
    match datum with
    | Atom (Boolean b) -> if b then "#t" else "#f"
    | Atom (Integer n) -> string_of_int n
    | Atom (Identifier id) -> Identifier.string_of_identifier id
    | Nil -> "()"
    | Cons (car, cdr) -> string_of_cons car cdr

  and string_of_cons car cdr =
    let rec strings_of_cons cdr =
      match cdr with
      | Nil -> []
      | Cons (car, cdr) -> (string_of_datum car) :: (strings_of_cons cdr)
      | _ -> ["."; string_of_datum cdr;] in
    let string_list = (string_of_datum car) :: (strings_of_cons cdr) in
    "(" ^ (String.concat " " string_list) ^ ")" in
  
  match value with
  | ValDatum (datum) -> string_of_datum datum
  | ValProcedure (ProcBuiltin p) -> "#<builtin>"
  | ValProcedure (ProcLambda (_, _, _)) -> "#<lambda>"
