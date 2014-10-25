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

(*Test Code Below*)

(* module Iterator : ITERATOR = struct

  
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
end *)
(*Test Code Above*)

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

(*
type 'a tree = Leaf | Node of ('a * 'a tree * 'a tree)

module type INORDER_TREE_ITERATOR = sig
  include ITERATOR
  (* parameters:  a tree t
   * returns:  an iterator that will yield the elements of t,
   *   each exactly once, in the order that would be produced
   *   by an in-order traversal of t. *)
  val create: 'a tree -> 'a t
end


 module InorderTreeIterator : INORDER_TREE_ITERATOR = struct
    type 'a t = 'a Stack.t
  exception NoResult

  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  
  let return_to = (Stack.create())

  let is_tree_visited (tre:'a tree) (tre_lst: 'a tree list):bool =
     List.exists (fun x -> (tre = x)) (tre_lst)

  (*returns (isleftleaf?,isrightleaf?,isleftvisited?,isrightvisited?*)
  let check_children (tre: 'a tree) (tre_lst: 'a tree list): bool * bool * bool * bool=
      match tre with
      |Leaf -> failwith "primary tree cannot be leaf"
      | Node(a,left,right) -> 
            match (left,right) with
            |(Leaf,Leaf) -> (true,true,(is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(Leaf,_) -> (true,false,(is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(_,Leaf) -> (false,true,(is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
            |(_,_) -> (false,false,(is_tree_visited left tre_lst), (is_tree_visited right tre_lst))
          
    
  let create (l: 'a tree) : 'a t =
    let rec traverser (current: 'a tree) (visited: 'a tree list) : 'a tree list =
      let checker_tup : bool*bool*bool*bool = check_children current visited in
        match (checker_tup,current) with
        |((true,true,false,false),Node(a,left,right)) -> if(Stack.is_empty return_to) then (current::visited)
                   else traverser (Stack.pop return_to) (current::visited)
        |((false,_,false,_),Node(a,left,right)) -> (Stack.push current return_to); traverser left visited
        |((_,false,_,false),Node(a,left,right)) -> (Stack.push current return_to); traverser right (current::visited)
        |((_,_,_,_),Node(a,left,right)) -> if(Stack.is_empty return_to) then (visited) 
                   else traverser (Stack.pop return_to) visited
        |((_,_,_,_), Leaf) -> failwith "primary tree cannot be leaf"
        in
   let lt : 'a tree list = [] in
   let visited_order : 'a tree list = traverser l lt in

   let acc : 'a t = Stack.create() in
   let rec create_helper (lst: 'a tree list)  =
      match lst with 
       Node(a,leaf,right)::tl -> (Stack.push a acc); create_helper tl
      | _ -> acc in
    create_helper visited_order
end  
 
*)
module type TAKE_ITERATOR = functor (I: ITERATOR) -> sig
  include ITERATOR

  (* parameters:  an integer n and an iterator i
   * returns:  an iterator that behaves the same as i for
   *   exactly n calls to next, but afterwards
   *   raises NoResult. *)
  val create: int -> 'a I.t -> 'a t
end

(*
module TakeIterator : TAKE_ITERATOR = functor (I : ITERATOR) -> struct
  
  exception NoResult


  type 'a t = 'a Stack.t

  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)


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
*)

module IteratorUtilsFn (I : ITERATOR) = struct
  open I
  type 'a t = 'a Stack.t
  exception NoResult

  let next (stack: 'a t) : 'a =
    try Stack.pop stack with Stack.Empty -> raise NoResult 

  let has_next (stack: 'a t) : bool =
    not(Stack.is_empty stack)

  (* effects: causes i to yield n results, ignoring
   *   those results.  Raises NoResult if i does.  *)
  let advance (n: int) (iter: 'a I.t) : unit =
    let rec advancer (current: int) : unit =
       if (current <= 0) then ()
     else I.next iter ; advancer (current - 1)
    advancer n
   
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

(* TODO:
module RangeIterator : RANGE_ITERATOR = functor (I : ITERATOR) -> struct
  ...
end
*)
