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
  match input with
  | Nil -> failwith "nil"
  | Atom (Identifier id) when Identifier.is_valid_variable id ->
     ExprVariable id
  | Atom (Identifier id) -> failwith "is a keyword"
  | Atom (Boolean bl) -> ExprSelfEvaluating (SEBoolean bl)
  | Atom (Integer intgr) -> ExprSelfEvaluating (SEInteger intgr)
  | Cons (first, second) ->
     (*match first with
       | Atom (Identifier id) when Identifier.is_valid_variable id ->
          ExprVariable id
       | Atom (Identifier id) -> when id = "quote" -> failwith "quote"
       | Atom (Identifier id) -> when id = "if" -> failwith "if"
       | Atom (Identifier id) -> when id = "lambda" -> failwith "lambda"
       | Atom (Identifier id) -> when id = "set!" -> failwith "set!"
       | Atom (Identifier id) -> when id = "let" -> failwith "let"
       | Atom (Identifier id) -> when id = "let*" -> failwith "let*"
       | Atom (Identifier id) -> when id = "letrec" -> failwith "letrec"
       | _ -> failwith "no match" *)
     (* Above match case didn't succeed, so id is not a valid variable. *)
     failwith "I'm such a huge fan!"
  | _ -> failwith "Everything you do is just amazing!"

(* Parses a datum into a toplevel input. toplevel = definition | expression. 
   so call read_expression and then if it fails, try definition. if that fails
   parsing failed.*) 
let read_toplevel (input : datum) : toplevel =
  ToplevelExpression (read_expression(input))
  (*match input with
  | _ -> failwith "Sing the Rowing Song!" *)

(* This function returns an initial environment with any built-in
   bound variables. *)
let rec initial_environment () : environment =
  let func_car (single_cell: value) : datum = 
    match single_cell) with 
    | ValDatum (el1, el2) -> el1
    | _ -> failwith "not a single con_cell" in
  let func_cdr (single_cell: value) : datum =
    match single_cell with
    | ValDatum (el1, el2) -> el2
    | ValDatum (el1) -> Nil
    | _ -> failwith "not a single con_cell" in
  let rec func_add (single_cell: value) : datum =
    match (car single_cell) with
    | ValDatum (Nil) -> 0
    | ValDatum (SEInteger x) -> (x + (func_add (cdr single_cell)))
    | _ -> failwith "must add integers" in
  
  let car = Identifier.variable_of_identifier(Identifier.identifier_of_string("car")) in
  let cdr = Identifier.variable_of_identifier(Identifier.identifier_of_string("cdr")) in
  let add = Identifier.variable_of_identifier(Identifier.identifier_of_string("+")) in
  let mult = Identifier.variable_of_identifier(Identifier.identifier_of_string("*")) in
  let is_eq = Identifier.variable_of_identifier(Identifier.identifier_of_string("equal?")) in
  let evl = Identifier.variable_of_identifier(Identifier.identifier_of_string("eval")) in

  let init_env = Environment.empty_environment in
  Environment.addbinding init_env (Identifier.variable_of_identifier("course": Identifier), 3110)


  (*use addbinding.
  [(car, func_car ); (cdr, func_cdr); (add, ); (mult, ); (is_eq, ); (evl, eval) ] *)

let self_eval_eval (se_expr: self_evaluating) : value =
  match se_expr with
  | SEInteger integer -> ValDatum (Atom (Integer integer))
  | SEBoolean boolean -> ValDatum (Atom (Integer boolean))


(* Evaluates an expression down to a value in a given environment. *)
(* You may want to add helper functions to make this function more
   readable, because it will get pretty long!  A good rule of thumb
   would be a helper function for each pattern in the match
   statement. *)
and eval (expression : expression) (env : environment) : value =
  match expression with
  | ExprSelfEvaluating se -> self_eval_eval
  | ExprVariable _        ->
     failwith "'Oh I sure love to row my boat with my...oar."
  | ExprQuote _           ->
     failwith "Rowing!"
  | ExprLambda (_, _)
  | ExprProcCall _        ->
     failwith "Sing along with me as I row my boat!'"
  | ExprIf (_, _, _) ->
     failwith "But I love you!"
  | ExprAssignment (_, _) ->
     failwith "Say something funny, Rower!"
  | ExprLet (_, _)
  | ExprLetStar (_, _)
  | ExprLetRec (_, _)     ->
     failwith "Ahahaha!  That is classic Rower."

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
