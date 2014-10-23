module type ITERATOR = sig
  type 'a t
  exception NoResult

  (* returns: true if there are more results to yield,
   *   otherwise returns false. *)
  val has_next: 'a t -> bool

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  val next: 'a t -> 'a
end

module type LIST_ITERATOR = sig
  include ITERATOR
  (* parameters:  a list l
   * returns:  an iterator that will yield the elements of l,the
   *   each exactly once, in the order that they appear in l,
   *   starting with the head.  *)
  val create: 'a list -> 'a t
end

module ListIterator : LIST_ITERATOR = struct

  
  type 'a t = 'a Stack.t
  exception NoResult

  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  let create (l: 'a list) : 'a t =
    let rev_l = List.rev(l) in
    let acc : 'a t = Stack.create() in
    let rec create_helper (lst: 'a list)  =
      match lst with 
       hd::tl -> (Stack.push hd acc); create_helper tl
      | _ -> acc in
    create_helper rev_l

end


type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end

(* 
 module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
    type 'a t = 'a Stack.t
  exception NoResult

  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  let create (l: 'a list) : 'a t =
    let rev_l = List.rev(l) in
    let acc : 'a t = Stack.create() in
    let rec create_helper (lst: 'a list)  =
      match lst with 
       hd::tl -> (Stack.push hd acc); create_helper tl
      | _ -> acc in
    create_helper rev_l
end  *)
 

module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end


module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  open I
  exception NoResult



  type 'a t = 'a Stack.t

  
  


  let next (stack: 'a t) : 'a = I.next

  let create (n: int) (iter : 'a I.t) : 'a t =
    

    let create_help (l: 'a list) : 'a t =
       let rev_l = List.rev(l) in
       let acc : 'a t = Stack.create() in
       let rec create_helper (lst: 'a list)  =
          match lst with 
          hd::tl -> (Stack.push hd acc); create_helper tl
          | _ -> acc in
       create_helper rev_l
    in
     let rec resizer (current: int) (accum: 'a list): 'a list =
        if (current <= 0) then accum
        else  resizer (current - 1) ((I.next iter)::accum)
      in

     let lst : 'a list = List.rev((resizer n [])) in
     (create_help lst)
end


module IteratorUtilsFn (I : ITERATOR) = struct
  open I

  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    failwith "Not implemented"

  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
    failwith "Not implemented"
end

module type RANGE_ITERATOR = functor (I : ITERATOR) -> sig
  include ITERATOR

  (* parameters: integers n and m and an iterator i
   * returns: an iterator that behaves the way I would
   *   on the nth through mth calls to next, and
   *   afterwards raises NoResult.
   *
   *   If n > m the resulting iterator should always raise NoResult.
   *)
  val create : int -> int -> 'a I.t -> 'a t
end

(* TODO:
module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  ...
end
*)
