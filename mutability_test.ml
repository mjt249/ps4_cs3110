open Mutability
open Assertions

let b = count_up_from 1 1
TEST_UNIT "count_up_from_test1" = assert_true ((b () ) = 1)
TEST_UNIT "count_up_from_test2" = assert_true ((b () ) = 2)
TEST_UNIT "count_up_from_test3" = assert_true ((b () ) = 3)

let c = count_up_from 1 2
TEST_UNIT "count_up_from_test1" = assert_true ((c () ) = 1)
TEST_UNIT "count_up_from_test2" = assert_true ((c () ) = 3)
TEST_UNIT "count_up_from_test3" = assert_true ((c () ) = 5)


let d = count_up_from 3 2
TEST_UNIT "count_up_from_test4" = assert_true ((d () ) = 3)
TEST_UNIT "count_up_from_test5" = assert_true ((d () ) = 5)
TEST_UNIT "count_up_from_test6" = assert_true ((d () ) = 7)


let () = Pa_ounit_lib.Runtime.summarize() 