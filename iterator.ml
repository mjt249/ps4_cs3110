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

(*Precondition: create takes an 'a list to make an iterator that will return
*the elements of the list one at a time.
*Postcondition: Will raise NoResult if next is called on an 
*iterator that is empty
*)
module ListIterator : LIST_ITERATOR = struct

  type 'a t = 'a Stack.t
  exception NoResult

  (*returns: stack has a next element to yield.*)
  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)
  
  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
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

(*Precondition: InorderTreeIterator takes an 'a tree.This initial 'a tree must
*not be a Leaf or the behavior is undefined.
*Postcondition: This iterator with give the 'a 's of the tree in 
*inorder traversal order
*)
 module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
    type 'a t = 'a Stack.t
  exception NoResult

  (*returns: stack has a next element to yield.*)
  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult           
    
  (*Precondition: InorderTreeIterator takes an 'a tree.This initial 'a tree must
  *not be a Leaf or the behavior is undefined.
  *Postcondition: This iterator with give the 'a 's of the tree in 
  *inorder traversal order*)
  let create (l: 'a tree) : 'a t =
    
  let is_tree_visited (tre:'a tree) (tre_lst: 'a tree list):bool =
     List.exists (fun x -> (tre = x)) (tre_lst)
   in
  (*returns (isleftleaf?,isrightleaf?,isleftvisited?,isrightvisited?*)
  let check_children (tre: 'a tree) (tre_lst: 'a tree list): 
     bool * bool * bool * bool=
      match tre with
      |Leaf -> failwith "primary tree cannot be leaf"
      | Node(a,left,right) -> 
            match (left,right) with
            |(Leaf,Leaf) -> (true,true,
              (is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(Leaf,_) -> (true,false,
              (is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(_,Leaf) -> (false,true,
              (is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(_,_) -> (false,false,
              (is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
   in

    let return_to = (Stack.create()) in  
    let rec traverser (current:'a tree) (visited:'a tree list):'a tree list =
      let checker_tup = check_children current visited in
        match (checker_tup,current) with
        |((true,true,false,false),Node(a,left,right)) -> 
           if(Stack.is_empty return_to) then (current::visited)
                   else traverser (Stack.pop return_to) (current::visited)
        |((false,_,false,_),Node(a,left,right)) -> 
          (Stack.push current return_to); traverser left visited
        |((_,false,_,false),Node(a,left,right)) -> 
          (Stack.push current return_to); traverser right (current::visited)
        |((_,_,_,_),Node(a,left,right)) -> 
           if(Stack.is_empty return_to) then (visited) 
                   else traverser (Stack.pop return_to) visited
        |((_,_,_,_), Leaf) -> failwith "primary tree cannot be leaf"
        in
   
   let visited_order = traverser l [] in

   let acc = Stack.create() in
   let rec create_helper (lst: 'a tree list)  =
      match lst with 
       Node(a,leaf,right)::tl -> (Stack.push a acc); create_helper tl
      | _ -> acc in
    create_helper visited_order
end  
 

module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end
(*Precondition: To create an iterator after applying Take ITerator
*to an ITERATOR module, pass int n and iter 'a I.t. n must be 0<= n <
*number of 'as or will raise NoResult
*Postcondition: returns:  an iterator that behaves the same as i for
*exactly n calls to next, but afterwards
*raises NoResult.
*)
module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  open I
  type 'a t = 'a Stack.t
  exception NoResult

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  (*returns: stack has a next element to yield.*)
  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack) 

  (*Precondition: To create an iterator after applying Take ITerator
  *to an ITERATOR module, pass int n and iter 'a I.t. n must be 0<= n <
  *number of 'as or will raise NoResult
  *Postcondition: returns:  an iterator that behaves the same as i for
  *exactly n calls to next, but afterwards
  *raises NoResult *)
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
(*Precondition: n must be 0<= n < nbr of as in i or
*may raise NoResult
*Postcondition: advances n and ignores those results
*)
module IteratorUtilsFn (I : ITERATOR) = struct
  type 'a t = 'a I.t
  (* requires: n is positive
       effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    let n_ref = ref n in
    while (!n_ref) <> 0 do
      n_ref := (!n_ref) -1;
      ignore (I.next iter)
   done
   
  (* returns: the final value of the accumulator after
   *   folding f over all the results returned by i,
   *   starting with acc as the initial accumulator.
   * effects: causes i to yield all its results. *)
  let rec fold (f : ('a -> 'b -> 'a)) (acc : 'a) (iter: 'b I.t) : 'a =
     if(I.has_next iter) then fold f (f acc (I.next iter)) iter
   else acc

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

(*Precondition:Create takes int n, int m, and iter requires:
* n <= m and n non-negative. 
*Postcondition: If the iter passed would normally produce 1,2,3,4,5,6
*and n = 2 and m = 4 then will return 3,4,5 instead.
*)
module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  module UtilApplied = IteratorUtilsFn(I)
  module TakeApplied = TakeIterator(I)

  type 'a t = 'a Stack.t
  exception NoResult

  (* returns:  the next result, if there are more results
   *   to yield.  Otherwise raises NoResult.
   * effects:  modifies the state of the iterator to record
   *   the yield. *)
  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  (*returns: stack has a next element to yield.*)
  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  let create_n (n: int) (iter : 'a I.t) : 'a t =
    
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

  (*requires: n <= m and n non-negative
   Precondition:Create takes int n, int m, and iter requires:
   n <= m and n non-negative. 
   Postcondition: If the iter passed would normally produce 1,2,3,4,5,6
   and n = 2 and m = 4 then will return 3,4,5 instead.*)
  let create (n: int) (m: int) (iter: 'a I.t): 'a t =
    UtilApplied.advance n iter;
    create_n (m-n + 1) iter


end

