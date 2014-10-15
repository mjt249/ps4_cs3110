let count_up_counter = ref 0
let next_val (counter: int ref) : unit -> int = fun () ->
  (counter := (!counter) +1;
  !counter)
let count_up_from n k =  fun () ->
  let i = next_val count_up_counter () in
  n + (i-1)*k

let tabulate f n =
  let result = Array.make n f in
  let tabulate_helper (index: int) (funct: int -> 'a) : 'a =
    funct index in
  Array.mapi tabulate_helper result

let fold_left_imp f acc xs = 
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
let zardoz (x:t) : u =
  el := !el + x;
  !el
