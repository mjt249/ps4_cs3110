open Iterator
open Assertions
(* open ListIterator  *)
open InorderTreeIterator

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
(* 
type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree) *)
let a = 1
let b = 2
let c = 3
let tre : 'a tree = Node(b,Node(a,Leaf,Leaf),Node(c,Leaf,Leaf))
let itr = InorderTreeIterator.create tre

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
TEST_UNIT "Next_8" =
assert_raises (Some (ListIterator.NoResult)) ListIterator.next itr2 


module Tester3 = RangeIterator(ListIterator)
let lst3 = [1;2;3]
let itr13 = ListIterator.create lst3
let itr3 = Tester3.create 0 2 itr13
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
assert_false (Tester3.has_next itr3) 
TEST_UNIT "Next_8" =
 assert_raises (Some (Tester3.NoResult)) Tester3.next itr3 


module Tester4 = RangeIterator(ListIterator)
let lst4 = [1;2;3;4;5;6;7;8]
let itr14 = ListIterator.create lst4
let itr4 = Tester4.create 1 3 itr14
TEST_UNIT "Next_1" =
assert_true (Tester4.has_next itr4)
TEST_UNIT "Next_2" =
assert_true ((Tester4.next itr4) = 2)
TEST_UNIT "Next_3" =
assert_true (Tester4.has_next itr4)
TEST_UNIT "Next_4" =
assert_true ((Tester4.next itr4) = 3)
TEST_UNIT "Next_5" =
assert_true (Tester4.has_next itr4)
TEST_UNIT "Next_6" =
assert_true ((Tester4.next itr4) = 4)
TEST_UNIT "Next_7" =
assert_false (Tester4.has_next itr4) 
TEST_UNIT "Next_8" =
assert_raises (Some (Tester4.NoResult)) Tester4.next itr4


module Tester5 = RangeIterator(ListIterator)
let lst5 = [1;2;3;4;5;6;7;8]
let itr15 = ListIterator.create lst5
let itr5 = Tester4.create 2 2 itr15
TEST_UNIT "Next_1" =
assert_true (Tester5.has_next itr5)
TEST_UNIT "Next_2" =
assert_true ((Tester5.next itr5) = 3)
TEST_UNIT "Next_3" =
assert_false (Tester5.has_next itr5)
TEST_UNIT "TEST CATCH" =
assert_raises (Some (Tester5.NoResult)) Tester5.next itr5 



let () = Pa_ounit_lib.Runtime.summarize() 
