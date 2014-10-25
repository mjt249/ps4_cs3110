open Iterator
open Assertions
open ListIterator 
open Iterator


 
let lst = [1;2;3]
let itr = create lst

TEST_UNIT "Next_1" =
assert_true (has_next itr)
TEST_UNIT "Next_2" =
assert_true ((next itr) = 1)
TEST_UNIT "Next_3" =
assert_true (has_next itr)
TEST_UNIT "Next_4" =
assert_true ((next itr) = 2)
TEST_UNIT "Next_5" =
assert_true (has_next itr)
TEST_UNIT "Next_6" =
assert_true ((next itr) = 3)
TEST_UNIT "Next_7" =
assert_false (has_next itr) 



module Tester = TakeIterator(Iterator)
let lst = [1;2;3]
let itr : int t = Tester.create 2 lst
TEST_UNIT "TAKE" =
assert_true (has_next) 

let () = Pa_ounit_lib.Runtime.summarize() 
