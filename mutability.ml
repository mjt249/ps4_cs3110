let count_up_counter = ref 0
let count_up_n = ref 0
let count_up_k = ref 0

let next_val (n: int) (k: int) (counter: int ref) : unit -> int = fun () ->
  if (((!count_up_n) = n) && ((!count_up_k) = k)) then
    (counter := (!counter) +1;
    !counter)
  else 
    (counter := 1;
    count_up_n := n;
    count_up_k := k;
    !counter)

(*requires: n,k positive ints. this is a loose condition because it will
  still work on non positive n and k, but wouldn't be 'counting up' anymore.
  The first time it is called, it should return n. 
  The second time f is called, it should return n+k. 
  More generally, the ith time f is called, it should return n+(i-1)*k.
  Every time the n or k change, the function will count up from n again.*)
let count_up_from (n: int) (k:int) =  fun () ->
  let i = next_val n k count_up_counter () in
  n + (i-1)*k

(*requires: non negative n. f is pure.
  returns an array of length n where the element at index i equals f i.*)
let tabulate (f: int -> 'a) (n: int) =
  let result = Array.make n f in
  let tabulate_helper (index: int) (funct: int -> 'a) : 'a =
    funct index in
  Array.mapi tabulate_helper result

(*fold_left_imp f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn.
  applies f on elements of xs from left to right while updatin the 
  accumulator with the value returned each time by f.*)
let fold_left_imp (f:'a -> 'b -> 'a) (acc: 'a) (xs: 'b list) : 'a = 
  let acc_ref = ref acc in
  let xs_ref = ref xs in
  while (!xs_ref) <> [] do
    match (!xs_ref) with
    | hd::tl -> acc_ref := f (!acc_ref) hd;
                xs_ref := tl
    | _ -> failwith "shouldn't have empty list"
   done;
   !acc_ref

type t = int
type u = int
let lst : t list = [1;2]
let el = ref 0
(*sets value of ref cell el to itself plus the argument. 
  returns the value of this addition/mutation.*)
let zardoz (x:t) : u =
  el := !el + x;
  !el
