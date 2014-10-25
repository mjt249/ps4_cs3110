open Assertions
open Eval
open Ast

let id1 = Identifier.identifier_of_string "test1"
let d1 = (Atom (Identifier id1))
TEST_UNIT "read_expression_test1" = assert_true ((read_expression d1) = 
	ExprVariable (Identifier.variable_of_identifier id1))

let id2 = Identifier.identifier_of_string "quote"
let d2 = (Atom (Identifier id2))
TEST_UNIT "read_expression_test2" = assert_raises (Some (Failure "is a keyword")) read_expression d2

let d3 = Atom (Boolean true)
TEST_UNIT "read_expression_test3" = assert_true ((read_expression d3) =
    ExprSelfEvaluating (SEBoolean true) )

let id4 = Identifier.identifier_of_string "quote"
let d4 = Cons ( Atom (Identifier id4), d1)
TEST_UNIT "read_expression_test4" = assert_true ((read_expression d4) =
    ExprQuote d1)

let id5 = Identifier.identifier_of_string "if"
let d5 = Cons ( Atom (Identifier id5), Cons (d3, (Cons (d3, Cons (d3, Nil))))) 
TEST_UNIT "read_expression_test5" = assert_true ((read_expression d5) =
	ExprIf (ExprSelfEvaluating (SEBoolean true), ExprSelfEvaluating (SEBoolean true), ExprSelfEvaluating (SEBoolean true)))
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
