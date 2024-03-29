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


(*requires : a datume of valid Scheme3110 syntax
  Parses a datum into an expression.
  Takes in a single datum, syntax check the datum using the expression syntax
  of scheme3110 as specified in the ps4 write up, and returns an expression. 
  If the datum is not valid Scheme3110 syntax, returns an error. The error will
  indicate that read failed for this datum. Does not include definitions*)
let rec read_expression (input : datum) : expression =
  let rec cons_to_expr_list (cons: datum) (acc: expression list) 
    : expression list =
    match cons with
    |Cons ( first, second ) -> cons_to_expr_list second 
       ((read_expression first)::acc) 
    |Nil -> List.rev(acc)
    | _ -> failwith "read failed: not a cons (to expr)" in 

  let rec cons_to_var_list (cons: datum) (acc: variable list): variable list =
    match cons with
    |Cons(Atom(Identifier id),second) when Identifier.is_valid_variable(id)-> 
        cons_to_var_list second ((Identifier.variable_of_identifier id)::acc) 
    |Nil -> List.rev(acc)
    | _ -> failwith "read failed: not a cons (to var)" in

  let rec cons_to_let_binding_list (cons: datum) (acc: let_binding list)
   : let_binding list =
    match cons with
    |Cons ( Cons( Atom (Identifier id), Cons(x,y)), rest) -> 
        cons_to_let_binding_list rest (((Identifier.variable_of_identifier id),
          (read_expression x)):: acc)
    |Nil -> List.rev(acc)
    | _ -> failwith "read failed: not a valid binding list" in

  let rec var_of_let_bindings (cons: datum) (acc: variable list):variable list=
    match cons with
    |Cons ( Cons( Atom (Identifier id), Cons(x,y)), rest) -> 
        var_of_let_bindings rest ((Identifier.variable_of_identifier id):: acc)
    |Nil -> acc
    | _ -> failwith "read failed: not a valid binding list" in    

  let rec check_dups (vars: variable list) (no_dups: bool) : bool=
    let rec check_tail (head: variable) (tail: variable list) (acc:bool):bool =
        match tail with
        | hd::tl -> check_tail head tl (acc && (hd<>head))
        | _ -> acc in
      match vars with 
      | hd::[] -> no_dups
      | hd::tl -> check_dups tl ((check_tail hd tl true) && no_dups)
      | _  -> no_dups in

  match input with
  |Atom (Identifier id) when Identifier.is_valid_variable id ->
     ExprVariable (Identifier.variable_of_identifier id)
  |Atom (Identifier id) -> failwith "is a keyword"
  |Atom (Boolean bl) -> ExprSelfEvaluating (SEBoolean bl)
  |Atom (Integer intgr) -> ExprSelfEvaluating (SEInteger intgr)
  |Cons (Atom (Identifier id), in_quote)
   when (id = (Identifier.identifier_of_string "quote")) -> 
      ExprQuote in_quote
  |Cons (Atom (Identifier id), Cons (e1, (Cons (e2, Cons (e3, Nil))))) 
   when (id = (Identifier.identifier_of_string "if")) ->
      ExprIf (read_expression e1, read_expression e2, read_expression e3)
  |Cons (Atom (Identifier id), Cons ( variables, expressions) ) 
   when (id = (Identifier.identifier_of_string("lambda"))) ->
      let var_list = cons_to_var_list variables [] in
      if (check_dups var_list true) then 
        ExprLambda (var_list, (cons_to_expr_list expressions []))
      else
        failwith "read failed: duplicate variables for lambda" 
  |Cons (Atom (Identifier id), the_rest) when Identifier.is_valid_variable id->
    ExprProcCall ( ExprVariable (Identifier.variable_of_identifier id), 
     cons_to_expr_list the_rest [] ) 
  |Cons (Cons (Atom (Identifier id), Cons(variables, expressions) ), arguments)
   when (id = (Identifier.identifier_of_string("lambda"))) ->
      ExprProcCall ( ExprLambda ((cons_to_var_list variables []), 
        (cons_to_expr_list expressions [])), (cons_to_expr_list arguments []))
  |Cons (Atom (Identifier id), Cons (Atom (Identifier var), Cons (expr, Nil)))
   when (id = Identifier.identifier_of_string("set!")) ->
    ExprAssignment((Identifier.variable_of_identifier var),read_expression expr)
  |Cons ( Atom (Identifier id), Cons (let_binds, expressions) ) 
   when (id = Identifier.identifier_of_string("let*")) ->
    ExprLetStar ((cons_to_let_binding_list let_binds []), 
      (cons_to_expr_list expressions []))
  |Cons ( Atom (Identifier id), Cons (let_binds, expressions) ) 
   when (id = Identifier.identifier_of_string("let")) ->
    if (check_dups (var_of_let_bindings let_binds []) true) then
      ExprLet ((cons_to_let_binding_list let_binds []), 
        (cons_to_expr_list expressions []))
    else 
      failwith "read failed: let cannot have duplicate variables"
  |Cons ( Atom (Identifier id), Cons (let_binds, expressions) ) 
    when (id = Identifier.identifier_of_string("letrec")) ->
      if (check_dups (var_of_let_bindings let_binds []) true) then
        ExprLetRec ((cons_to_let_binding_list let_binds []), 
          (cons_to_expr_list expressions []))
      else
        failwith "read failed: letrec cannot have duplicate variables"

  | _ -> failwith "read failed: no match"

(*requires : a datume of valid Scheme3110 syntax
  Parses a datum into a program, or one top level phrase. 
  Takes in a single datum, syntax check the datum using the expression syntax
  of scheme3110 as specified in the ps4 write up, returns a top level phrase. 
  If the datum is not valid Scheme3110 syntax, returns an error. The error will
  indicate that read failed for this datum. Includes expressions as well as
  definitions.*)
let read_toplevel (input : datum) : toplevel =
  match input with
  | Cons (Atom (Identifier id), Cons( Atom (Identifier var), Cons(expr, Nil))) 
     when (id = (Identifier.identifier_of_string("define")))-> 
      ToplevelDefinition ((Identifier.variable_of_identifier var), 
      read_expression expr)
  | _ -> ToplevelExpression (read_expression input )

(*returns an initial environment with all built in bindings.
  They are course, car, cdr, cons, +, *, equal?, eval*)
let rec initial_environment () : environment =

  (*single cell is a value list with exactly one cons cell*)
  let func_car (single_cell: value list) (env: environment): value = 

    match single_cell with
    | (ValDatum (Cons (d1, d2)))::[] -> ValDatum d1
    | _ -> failwith "Invalid arguments to car." in
  (*single cell is a value list with exactly one cons cell*)
  let func_cdr (single_cell: value list) (env: environment) : value=
    match single_cell with
    | (ValDatum (Cons (d1, d2)))::[] -> ValDatum d2
    | _ -> failwith "Invalid arguments to cdr." in

  let func_cons (two_datum : value list) (env: environment) : value =
    match two_datum with
    | (ValDatum d1)::(ValDatum d2)::[] -> ValDatum (Cons (d1, d2))
    | _ -> failwith "Invalid arguments to cons." in

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

  let cons_ref = ref (ValProcedure (ProcBuiltin (func_cons))) in
  let cons_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("cons")) in
  let cons_env =  Environment.add_binding cdr_env (cons_var, cons_ref) in

  let add_ref = ref (ValProcedure (ProcBuiltin (func_add))) in
  let add_var = Identifier.variable_of_identifier(Identifier.identifier_of_string("+")) in
  let add_env =  Environment.add_binding cons_env (add_var, add_ref) in

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
   statement. 
  Evaluation happens in the context of a dynamic environment 
  in which bindings may be mutated. Rules for Scheme3110 evaluation are
  given in the write up for ps4. Error messages detail which of the rules
  the expression violated when evaluating in that environment.*)
and eval (expression : expression) (env : environment) : value =
  let rec expr_list_to_val_list (e_list: expression list) (acc: value list)
   : value list =
    match e_list with 
    | hd::tl -> expr_list_to_val_list tl ((eval hd env)::acc)
    | _ -> List.rev(acc) in

  let self_eval_eval (se_expr: self_evaluating) : value =
    match se_expr with
    | SEInteger integer -> ValDatum (Atom (Integer integer))
    | SEBoolean boolean -> ValDatum (Atom (Boolean boolean)) in

  let variable_eval (var: variable) (temp_env: environment) : value =
    if (Environment.is_bound temp_env var) then
      !(Environment.get_binding temp_env var) 
    else 
      failwith ("variable "^(Identifier.string_of_variable var)^" not bound")in

  let quote_eval (quote_expr : datum) : value =
    match quote_expr with
    | Cons (first, second) -> ValDatum first
    | _ -> failwith "read_expression went wrong for quote. cannot eval" in

  let if_eval (e1: expression) (e2: expression) (e3: expression) 
   (env: environment) : value =
    if ((eval e1 env) = ValDatum ( Atom (Boolean false))) then 
      (eval e3 env) 
    else (eval e2 env) in

  let lambda_eval (variables: variable list) 
    (expressions: expression list) : value =
      ValProcedure (ProcLambda (variables, env, expressions)) in

(*for letstar : in order*)
  let rec add_temp_bindings (temp_env: environment) (variables: variable list)
  (arguments: expression list) : environment =
    match variables, arguments with
    | var::vars, arg::args -> 
        let arg_ref = ref (eval arg temp_env) in
        add_temp_bindings 
          (Environment.add_binding temp_env (var, arg_ref)) vars args
    | [], [] -> temp_env 
    | _ -> failwith "wrong number of arguments to let* procedure call" in

(*for letrec and lambda*)
  let rec simultaneous_add_temp_bindings (temp_env: environment) 
  (variables: variable list) (arguments: expression list) : environment =
    match variables, arguments with
    | var::vars, arg::args -> 
        let arg_ref = ref (eval arg env) in
        simultaneous_add_temp_bindings 
          (Environment.add_binding temp_env (var, arg_ref)) vars args
    | [], [] -> temp_env 
    | _ -> failwith "wrong number of arguments to lambda/letrec proc call"in

  let assign_eval (var: variable) (expr: expression) : value =
    if (Environment.is_bound env var) then 
      let var_binding = (Environment.get_binding env var) in
      var_binding := (eval expr env);
      ValDatum Nil
    else 
      failwith ("variable "^(Identifier.string_of_variable var)^
          " must already be bound") in

  let rec proc_lambda_eval_helper (expr_list: expression list) 
   (temp_env: environment) (acc:value): value=
      match expr_list with
      | hd::tl -> proc_lambda_eval_helper tl temp_env (eval hd temp_env)
      | _ -> acc in 

  let proc_lambda_eval  (temp_env: environment) (expressions: expression list) 
   : value =
    proc_lambda_eval_helper expressions temp_env (ValDatum Nil) in


  let rec separate_bindings (bind_l: let_binding list)
   (acc: variable list * expression list) =
    match bind_l with
    | (var,exp)::tl -> separate_bindings tl (var::(fst acc), exp::(snd acc))
    | [] -> acc in

  let let_star (bind_lst: let_binding list) (expr: expression list) : value =
     
    let not_reversed_sep = (separate_bindings bind_lst ([],[])) in
    let sep_binds = ((List.rev(fst not_reversed_sep)), 
      (List.rev(snd not_reversed_sep))) in
    let temp_env = add_temp_bindings env (fst sep_binds) (snd sep_binds) in
     proc_lambda_eval_helper expr 
       (Environment.combine_environments env temp_env) (ValDatum Nil) in
  
  let rec var_lst_maker (vlst: variable list * expression list) 
    (new_vlists: variable list * expression list) =
    match vlst with
      |(hd::tl,hd2::tl2) -> var_lst_maker (tl,tl2) ((hd::(fst new_vlists)),
         (hd2::(snd new_vlists)))
      | ([],[]) -> new_vlists 
      | _ -> failwith "wrong number of arguments" in 

  let func_let (bind_lst: let_binding list) ( expr: expression list) : value =
    let not_reversed_sep = (separate_bindings bind_lst ([],[])) in
    let checked_for_dups = var_lst_maker not_reversed_sep ([],[]) in
    proc_lambda_eval (simultaneous_add_temp_bindings env (fst checked_for_dups)
     (snd checked_for_dups)) expr in


  let rec init_bindings (temp_env: environment) (variables: variable list) 
   : environment =
    match variables with
    | var::vars -> 
        let arg_ref = ref (ValDatum Nil) in
        init_bindings (Environment.add_binding temp_env (var, arg_ref)) vars 
    | [] -> temp_env in

  let rec assign_bindings (temp_env:environment) (variables:variable list)
   (arguments: expression list) : environment =
    match variables, arguments with
    | var::vars, arg::args ->
        if (Environment.is_bound temp_env var) then 
          let var_binding = (Environment.get_binding temp_env var) in
          var_binding := (eval arg temp_env);
          assign_bindings temp_env vars args
        else 
          failwith ("variable "^(Identifier.string_of_variable var)^
            " must already be bound:letrec") 
    | [], [] -> temp_env
    | _ -> failwith "assign_bindings went wrong" in

  let all_others_visible (temp_env: environment) (variables: variable list)
  (arguments: expression list) : environment =
    let add_all_env = init_bindings temp_env variables in
    assign_bindings add_all_env variables arguments in

  let func_letrec (bind_lst: let_binding list) (expr: expression list) : value =
    let not_reversed_sep = (separate_bindings bind_lst ([],[])) in
    let checked_for_dups = var_lst_maker not_reversed_sep ([],[]) in
    proc_lambda_eval (all_others_visible env (fst checked_for_dups) 
      (snd checked_for_dups)) expr in

  match expression with
  |ExprSelfEvaluating se -> self_eval_eval se
  |ExprVariable variable -> variable_eval variable env
  |ExprQuote datum       -> quote_eval datum
  |ExprLambda (var_list, expr_list) -> lambda_eval var_list expr_list
  |ExprProcCall (ExprLambda (var_list, expr_list), arguments) -> 
    proc_lambda_eval (add_temp_bindings env var_list arguments) expr_list
  |ExprProcCall (ExprVariable var, e_list) -> 
    if (Environment.is_bound env var) then
      match (!(Environment.get_binding env var)) with
      |ValProcedure (ProcBuiltin builtin) -> 
         builtin (expr_list_to_val_list e_list []) env
      |ValProcedure (ProcLambda (var_list, temp_env, expr_list)) -> 
         proc_lambda_eval 
           (simultaneous_add_temp_bindings temp_env var_list e_list) expr_list
      | _ -> failwith "datum argument in procedure call"
      else 
        failwith ((Identifier.string_of_variable var)^" is not bound")
  |ExprIf (e1, e2, e3) -> if_eval e1 e2 e3 env
  |ExprAssignment (var, expr) -> assign_eval var expr
  |ExprLet (let_bindlst, exprs) -> func_let let_bindlst exprs
  |ExprLetStar (let_bindlst, exprs) -> let_star let_bindlst exprs
  |ExprLetRec (let_bindlst, exprs)  -> func_letrec let_bindlst exprs
  | _ -> failwith "no match"

(* Evaluates a top level phrase down to a value and an output environment in a
   given environment. The phrase may may be either expressions or definitions. 
   Top-level evaluation therefore takes in an toplevel (which is either a 
   definition or an expression) and an environment to evaluate it in, 
   and returns a value and an environment with any new bindings that should be
   visible to future evaluations.
   Again, rules are given in the write up for ps4 and error messages details
   which of the rules the toplevel phrase violates.*)
let eval_toplevel (toplevel : toplevel) (env : environment) 
  : value * environment = 
  let definition_eval (var: variable) (expr: expression): value * environment = 
    let check_var (v: variable) : environment =
      if (Environment.is_bound env v) then
        env
      else 
        let bind_v = ref (ValDatum Nil) in
        Environment.add_binding env (v, bind_v) in
    let temp_env = check_var var in
    let var_binding = (Environment.get_binding temp_env var) in
    var_binding := (eval expr temp_env);
    ( ValDatum Nil, temp_env) in

  match toplevel with
  | ToplevelExpression expression -> (eval expression env, env)
  | ToplevelDefinition (var, expr) ->  definition_eval var expr 

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
