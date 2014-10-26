open Assertions
open Eval
open Ast
(* 
  | ExprSelfEvaluating of self_evaluating
  | ExprVariable       of variable
  | ExprQuote          of datum
  | ExprLambda         of variable list * expression list
  | ExprProcCall       of expression * expression list
  | ExprIf             of expression * expression * expression
  | ExprAssignment     of variable * expression
  | ExprLet            of let_binding list * expression list
  | ExprLetStar        of let_binding list * expression list
  | ExprLetRec         of let_binding list * expression list
 *)

let id1 = Identifier.identifier_of_string "test1"
let variable_pre = (Atom (Identifier id1))
let variable_after = ExprVariable (Identifier.variable_of_identifier id1)
TEST_UNIT "read_expression_test1" = assert_true ((read_expression variable_pre) = 
	variable_after)

let id2 = Identifier.identifier_of_string "quote"
let quote_pre = (Atom (Identifier id2))
TEST_UNIT "read_expression_test2" = assert_raises (Some (Failure "is a keyword")) read_expression quote_pre

let true_pre = Atom (Boolean true)
let true_after = ExprSelfEvaluating (SEBoolean true) 
TEST_UNIT "read_expression_test3" = assert_true ((read_expression true_pre) =
    true_after)

let quote_id = Identifier.identifier_of_string "quote"
let quote_pre = Cons ( Atom (Identifier quote_id), variable_pre)
let quote_after = ExprQuote variable_pre
TEST_UNIT "read_expression_test4" = assert_true ((read_expression quote_pre) =
    quote_after)

let if_id = Identifier.identifier_of_string "if"
let if_pre = Cons ( Atom (Identifier if_id), 
	Cons (true_pre, (Cons (true_pre, Cons (true_pre, Nil))))) 
let if_after = ExprIf (ExprSelfEvaluating (SEBoolean true), ExprSelfEvaluating (SEBoolean true), ExprSelfEvaluating (SEBoolean true))
TEST_UNIT "read_expression_test5" = assert_true ((read_expression if_pre) =
	if_after)

let lambda_id = Identifier.identifier_of_string "lambda"
let lambda_pre =   Parser.main Lexer.token (Lexing.from_string "(lambda (test1) test1)") 
let lambda_after = ExprLambda ([Identifier.variable_of_identifier(id1)] , [ExprVariable (Identifier.variable_of_identifier(id1))])
TEST_UNIT "read_expression_test_6" = assert_true ((read_expression (List.hd(lambda_pre))) = lambda_after)

 (*TEST_UNIT "read_expression_test6" = assert_true ()
TEST_UNIT "read_expression_test7" = assert_true ()
TEST_UNIT "read_expression_test8" = assert_true ()
TEST_UNIT "read_expression_test9" = assert_true ()
TEST_UNIT "read_expression_test10" = assert_true ()
TEST_UNIT "read_expression_test11" = assert_true ()
TEST_UNIT "read_expression_test12" = assert_true ()
TEST_UNIT "read_expression_test13" = assert_true ()
TEST_UNIT "read_expression_test14" = assert_true ()
TEST_UNIT "read_expression_test15" = assert_true ()
TEST_UNIT "read_expression_test16" = assert_true ()
TEST_UNIT "read_expression_test17" = assert_true ()
TEST_UNIT "read_expression_test18" = assert_true () *)

let () = Pa_ounit_lib.Runtime.summarize() 
