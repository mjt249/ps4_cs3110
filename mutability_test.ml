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
TEST_UNIT "count_up_from_test7" = assert_true ((b () ) = 1)
TEST_UNIT "count_up_from_test8" = assert_true ((c () ) = 1)

TEST_UNIT "tabulate_test1" = assert_true ((tabulate (fun x -> x) 0) = [||])
TEST_UNIT "tabulate_test2" = assert_true ((tabulate (fun x -> x*x) 5) = [|0; 1; 4; 9; 16|])
TEST_UNIT "tabulate_test3" = assert_true ((tabulate (fun x -> 1) 2) = [|1; 1|])
TEST_UNIT "tabulate_test4" = assert_true ((tabulate (tabulate (fun x -> x)) 3) =
  [|[||]; [|0|]; [|0; 1|]|]) 
TEST_UNIT "tabulate_test5" = assert_true ((tabulate (fun x -> ()) 4) = [|();();();()|])

TEST_UNIT "fold_left_imp_test1" = assert_true ((fold_left_imp (+) 0 [1;1;1]) = 3)
TEST_UNIT "fold_left_imp_test2" = assert_true ((fold_left_imp ( * ) 0 [1;1;1]) = 0)
TEST_UNIT "fold_left_imp_test3" = assert_true ((fold_left_imp (^) "" ["a";"b";"c"]) = "abc")
TEST_UNIT "fold_left_imp_test4" = assert_true ((fold_left_imp (fun acc x-> x::acc) [] [1;2;3]) = [3;2;1])
TEST_UNIT "fold_left_imp_test5" = assert_true ((fold_left_imp (fun a _ -> a+1) 0 []) = 0)

TEST_UNIT "zardoz_inequality" = assert_true ((List.map zardoz (List.rev lst)) <> (List.rev (List.map zardoz lst)))

let () = Pa_ounit_lib.Runtime.summarize() 