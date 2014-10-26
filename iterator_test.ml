open Iterator
open Assertions
open ListIterator 


(* 
 
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
 *)


module Tester = TakeIterator(ListIterator)
let lst = [1;2;3]
let itr1 = ListIterator.create lst
let itr  = Tester.create 3 itr1
TEST_UNIT "TAKE" =
assert_true (Tester.has_next itr) 
TEST_UNIT "Next_1" =
assert_true (Tester.has_next itr)
TEST_UNIT "Next_2" =
assert_true ((Tester.next itr) = 1)
TEST_UNIT "Next_3" =
assert_true (Tester.has_next itr)
TEST_UNIT "Next_4" =
assert_true ((Tester.next itr) = 2)
TEST_UNIT "Next_5" =
assert_true (Tester.has_next itr)
TEST_UNIT "Next_6" =
assert_true ((Tester.next itr) = 3)
TEST_UNIT "Next_7" =
assert_false (Tester.has_next itr) 

module Tester2 = IteratorUtilsFn(ListIterator)
let lst = [8;8;1;2;3]
let itr2  = ListIterator.create lst
let () = Tester2.advance 2 itr2
TEST_UNIT "TAKE" =
assert_true (ListIterator.has_next itr2) 
TEST_UNIT "Next_1" =
assert_true (ListIterator.has_next itr2)
TEST_UNIT "Next_2" =
assert_true ((ListIterator.next itr2) = 1)
TEST_UNIT "Next_3" =
assert_true (ListIterator.has_next itr2)
TEST_UNIT "Next_4" =
assert_true ((ListIterator.next itr2) = 2)
TEST_UNIT "Next_5" =
assert_true (ListIterator.has_next itr2)
TEST_UNIT "Next_6" =
assert_true ((ListIterator.next itr2) = 3)
TEST_UNIT "Next_7" =
assert_false (ListIterator.has_next itr2) 
(* 
module Tester3 = RangeIterator(ListIterator)
let lst3 = [1;2;3]
let itr3 = ListIterator.create lst
let itr3 = Tester.create 0 2 itr3
TEST_UNIT "TAKE" =
assert_true (Tester3.has_next itr3) 
TEST_UNIT "Next_1" =
assert_true (Tester3.has_next itr3)
TEST_UNIT "Next_2" =
assert_true ((Tester3.next itr3) = 1)
TEST_UNIT "Next_3" =
assert_true (Tester3.has_next itr3)
TEST_UNIT "Next_4" =
assert_true ((Tester3.next itr3) = 2)
TEST_UNIT "Next_5" =
assert_true (Tester3.has_next itr3)
TEST_UNIT "Next_6" =
assert_true ((Tester3.next itr3) = 3)
TEST_UNIT "Next_7" =
assert_false (Tester3.has_next itr3)  *)



let () = Pa_ounit_lib.Runtime.summarize() 
