(*
                         CS 51 Problem Set 4
                Modules, Functors, and Priority Queues
                           Priority Queues
                             Spring 2017
*)

open Order

open Orderedcoll ;;

(*======================================================================
Section 3: Priority queues

A signature for a priority queue. See the problem set specification
for more information about priority queues.

IMPORTANT: In your implementations of priority queues, the MINIMUM
valued element corresponds to the HIGHEST priority. For example, in an
integer priority queue, the integer 4 has lower priority than the
integer 2. In case of multiple elements with identical priority, the
priority queue returns them according to the normal queue discipline,
first-in-first-out.
......................................................................*)

module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* The type of elements being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Returns whether or not a queue is empty *)
  val is_empty : queue -> bool

  (* Returns a new queue with the element added *)
  val add : elt -> queue -> queue

  (* Returns a pair of the highest priority element in the argument
     queue and the queue with that element removed. In case there is
     more than one element with the same priority (that is, the
     elements compare Equal), returns the one that was added
     first. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue

  (* Returns a string representation of the queue *)
  val to_string : queue -> string

  (* Runs invariant checks on the implementation of this binary tree.
     May raise Assert_failure exception *)
  val run_tests : unit -> unit

end

(*......................................................................
Problem 2: Implementing ListQueue

Implement a priority queue using a simple list to store the elements
in sorted order. Feel free to use anything from the List module.

After you've implemented ListQueue, you'll want to test the functor
by, say, generating an IntString priority queue and running the tests
to make sure your implementation works.
......................................................................*)

module ListQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    exception QueueEmpty

    type elt = C.t

    type queue = elt list

    let empty : queue = []

    let is_empty (q : queue) : bool =
      q = empty

    (* Adds elt to queue so that it's behind preexisting values < or = to it *)
    let rec add (e : elt) (q : queue) : queue =
      match q with
      | [] -> [e]
      | hd :: tl -> 
          match C.compare e hd with
          | Less -> e :: q
          | Equal | Greater -> hd :: add e tl 

    let take (q : queue) : elt * queue =
      match q with
      | [] -> raise QueueEmpty
      | hd :: tl -> hd, tl

    (* IMPORTANT: Don't change the implementation of to_string. *)
    let to_string (q: queue) : string =
      let rec to_string' q =
        match q with
        | [] -> ""
        | [hd] -> (C.to_string hd)
        | hd :: tl -> (C.to_string hd) ^ ";" ^ (to_string' tl)
      in
      let qs = to_string' q in "[" ^ qs ^ "]"  

    let test_empty_and_is_empty () = 
      let q = empty in
      assert (q = []);
      assert (is_empty q);
      let x = C.generate () in
      let q = add x q in 
      assert (not (is_empty q));
      let _, q = take q in
      assert (is_empty q)

    let test_add () =
      let x = C.generate () in
      let q = add x empty in 
      assert (q = [x]);
      let q = add x q in
      assert (q = [x; x]);
      let y = C.generate_gt x in
      let q = add y q in
      assert (q = [x; x; y]);
      let z = C.generate_lt x in
      let q = add z q in
      assert (q = [z; x; x; y]);
      let y' = C.generate_gt y in
      let q = add y' q in
      assert (q = [z; x; x; y; y']);
      let z' = C.generate_lt z in
      let q = add z' q in
      assert (q = [z'; z; x; x; y; y'])

    let test_take () =
      let x = C.generate () in
      let x' = x in
      let x2 = C.generate_gt x in
      let x3 = C.generate_gt x2 in
      let x4 = C.generate_gt x3 in
      assert (take (add x4 (add x3 (add x2 (add x (add x' empty))))) 
        = (x', [x; x2; x3; x4]));
      assert (take (add x4 (add x3 (add x2 (add x empty)))) = (x, [x2;x3;x4]));
      assert (take (add x4 (add x3 (add x2 empty))) = (x2, [x3; x4]));
      assert (take (add x4 (add x3 empty)) = (x3, [x4]));
      assert (take (add x4 empty) = (x4, empty))

    let run_tests () =
      test_empty_and_is_empty ();
      test_add ();
      test_take ();
      ()

  end

(*......................................................................
Problem 3: Implementing TreeQueue

Now implement a functor TreeQueue that generates implementations of
the priority queue signature PRIOQUEUE using a binary search tree.
Luckily, you should be able to use *a lot* of your code from the work
with BinSTree!

If you run into problems implementing TreeQueue, you can at least
add stub code for each of the values you need to implement so that
this file will compile and will work with the unit testing
code. That way you'll be able to submit the problem set so that it
compiles cleanly.
......................................................................*)

module TreeQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    exception QueueEmpty

    (* You can use the module T to access the functions defined in BinSTree,
       e.g. T.insert *)
    module T = (BinSTree(C) : (ORDERED_COLLECTION with type elt = C.t))

    (* Implement the remainder of the module. *)

    type elt = C.t

    type queue = T.collection

    let empty : queue = T.empty

    let is_empty (q : queue) : bool =
      q = empty

    let add (e : elt) (q : queue) : queue =
      T.insert e q

    let take (q : queue) : elt * queue =
      let open T in
      getmin q, delete (getmin q) q

    let to_string (q: queue) : string =
      T.to_string q    

    let test_empty_and_is_empty () = 
      let q = empty in
      assert (q = T.empty);
      assert (is_empty q);
      let x = C.generate () in
      let q = add x q in 
      assert (not (is_empty q));
      let _, q = take q in
      assert (is_empty q)  

    let test_add () =
      let open T in
      let x = C.generate () in
      let q = add x empty in
      assert (search x q);
      let x' = x in 
      let q = add x' q in
      assert (search x' q);
      assert (getmin q = x);
      assert (getmax q = x);
      let y = C.generate_gt x in
      let q = add y q in
      assert (search y q);
      let y' = y in
      let q = add y' q in
      assert (search y' q); 
      assert (getmax q = y);  
      let z = C.generate_lt x in
      let q = add z q in
      assert (search z q);
      let z' = z in
      let q = add z' q in
      assert (search z' q); 
      assert (getmin q = z)  

    let test_take () = 
      let x = C.generate () in
      let x' = x in
      let x2 = C.generate_gt x in
      let x3 = C.generate_gt x2 in
      let x4 = C.generate_gt x3 in
      assert (take (add x4 (add x3 (add x2 (add x (add x' empty))))) 
        = (x', (add x4 (add x3 (add x2 (add x empty))))));
      assert (take (add x4 (add x3 (add x2 (add x empty)))) 
        = (x, (add x4 (add x3 (add x2 empty)))));
      assert (take (add x4 (add x3 (add x2 empty))) 
        = (x2, (add x4 (add x3 empty))));
      assert (take (add x4 (add x3 empty)) 
        = (x3, (add x4 empty)));
      assert (take (add x4 empty) = (x4, empty))

    let run_tests () =
      test_empty_and_is_empty ();
      test_add ();
      test_take ();
      ()

  end

(*......................................................................
Problem 4: Implementing BinaryHeap

Implement a priority queue using a binary heap. See the problem set
writeup for more info.

You should implement a min-heap, i.e., the top of your heap stores the
smallest element in the entire heap.

Note that, unlike for your tree and list implementations of priority
queues, you do *not* need to worry about the order in which elements
of equal priority are removed. Yes, this means it's not really a
"queue", but it is easier to implement without that restriction.

Be sure to read the pset spec for hints and clarifications!

Remember the invariants of the tree that make up your queue:

1) A tree is ODD if its left subtree has 1 more node than its right
subtree. It is EVEN if its left and right subtrees have the same
number of nodes. The tree can never be in any other state. This is the
WEAK invariant, and should never be false.

2) All nodes in the subtrees of a node should be *greater* than (or
equal to) the value of that node.  This, combined with the previous
invariant, makes a STRONG invariant.  Any tree that a user passes in
to your module and receives back from it should satisfy this
invariant.  However, in the process of, say, adding a node to the
tree, the tree may intermittently not satisfy the order invariant. If
so, you *must* fix the tree before returning it to the user.  Fill in
the rest of the module below!
......................................................................*)
   
module BinaryHeap (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct

    exception QueueEmpty

    type elt = C.t

    (* A node in the tree is either even or odd *)
    type balance = Even | Odd

    (*
     A tree either:
       1) is one single element,

       2) has one branch, where:
          the first elt in the tuple is the element at this node,
          and the second elt is the element down the branch,

       3) or has two branches (with the node being even or odd)
    *)
    type tree =
    | Leaf of elt
    | OneBranch of elt * elt
    | TwoBranch of balance * elt * tree * tree

    (* A queue is either empty, or a tree *)
    type queue = Empty | Tree of tree

    let empty = Empty

    (* Prints binary heap as a string - nice for testing! *)
    let to_string (q: queue) =
      let rec to_string' (t: tree) =
        match t with
        | Leaf e1 -> "Leaf " ^ C.to_string e1
        | OneBranch(e1, e2) ->
                 "OneBranch (" ^ C.to_string e1 ^ ", "
                 ^ C.to_string e2 ^ ")"
        | TwoBranch(Odd, e1, t1, t2) ->
                 "TwoBranch (Odd, " ^ C.to_string e1 ^ ", "
                 ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
        | TwoBranch(Even, e1, t1, t2) ->
                 "TwoBranch (Even, " ^ C.to_string e1 ^ ", "
                 ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
      in
      match q with
      | Empty -> "Empty"
      | Tree t -> to_string' t

    let is_empty (q : queue) = q = Empty

    (* Adds element e to the queue q *)
    let add (e : elt) (q : queue) : queue =
      (* Given a tree, where e will be inserted is deterministic based
         on the invariants. If we encounter a node in the tree where
         its value is greater than the element being inserted, then we
         place the new elt in that spot and propagate what used to be
         at that spot down toward where the new element would have
         been inserted *)
      let rec add_to_tree (e : elt) (t : tree) : tree =
        match t with
        (* If the tree is just a Leaf, then we end up with a OneBranch *)
        | Leaf e1 ->
                 (match C.compare e e1 with
                  | Equal | Greater -> OneBranch (e1, e)
                  | Less -> OneBranch (e, e1))

        (* If the tree was a OneBranch, it will now be a TwoBranch *)
        | OneBranch(e1, e2) ->
                 (match C.compare e e1 with
                  | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
                  | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))

        (* If the tree was even, then it will become an odd tree (and
           the element is inserted to the left *)
        | TwoBranch(Even, e1, t1, t2) ->
                 (match C.compare e e1 with
                  | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
                  | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))

        (* If the tree was odd, then it will become an even tree (and
           the element is inserted to the right *)
        | TwoBranch(Odd, e1, t1, t2) ->
                 match C.compare e e1 with
                 | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
                 | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
      in
      (* If the queue is empty, then e is the only Leaf in the tree.
         Else, insert it into the proper location in the pre-existing
         tree *)
      match q with
      | Empty -> Tree (Leaf e)
      | Tree t -> Tree (add_to_tree e t)

    (*..................................................................
    Simply returns the top element of the tree t (i.e., just a single
    pattern match)
    ..................................................................*)
    let get_top (t : tree) : elt =
      match t with
      | Leaf e1 | OneBranch(e1, _) | TwoBranch(_, e1, _, _) -> e1

    let test_get_top () = 
      let x = C.generate () in
      let y = C.generate () in
      assert (get_top (Leaf x) = x);
      assert (get_top (OneBranch(x, y)) = x);
      assert (get_top (OneBranch(y, x)) = y);
      assert (get_top (TwoBranch(Odd, x, Leaf y, Leaf y)) = x);
      assert (get_top (TwoBranch(Even, y, Leaf x, Leaf x)) = y);
      assert (get_top (TwoBranch(Odd, x, OneBranch(y, y), Leaf y)) = x);
      assert (get_top (TwoBranch(Even, y, Leaf x, OneBranch(x, x))) = y)
          
    (*..................................................................
    Takes a tree, and if the top node is greater than its children,
    fixes it. If fixing it results in a subtree where the node is
    greater than its children, then you must (recursively) fix this
    tree too.
    ..................................................................*)
    let rec fix (t : tree) : tree =
      let replace_top element tr =
        match tr with 
        | Leaf _ -> Leaf element
        | OneBranch(_, e2) -> OneBranch(element, e2)
        | TwoBranch(b, _, t1, t2) -> TwoBranch(b, element, t1, t2) 
      in
      match t with 
      | Leaf e1 -> Leaf e1
      | OneBranch(e1, e2) ->
          (match C.compare e1 e2 with
           | Equal | Less -> OneBranch(e1, e2)
           | Greater -> OneBranch(e2, e1))
      | TwoBranch(b, e1, t1, t2) ->
          match C.compare (get_top t1) (get_top t2) with 
          | Equal | Less ->
              (match C.compare e1 (get_top t1) with                      
               | Equal | Less -> TwoBranch(b, e1, t1, t2)
               | Greater ->
                   TwoBranch(b, get_top t1, fix (replace_top e1 t1), t2))
          | Greater ->
              match C.compare e1 (get_top t2) with                      
              | Equal | Less -> TwoBranch(b, e1, t1, t2)
              | Greater ->
                  TwoBranch(b, get_top t2, t1, fix(replace_top e1 t2))

    let test_fix () =
      let x = C.generate () in
      let x' = x in
      let y = C.generate_gt x in
      let y' = y in
      let z = C.generate_lt x in
      let z' = z in
      assert (fix (Leaf x) = Leaf x);
      assert (fix (OneBranch(x, x')) = (OneBranch(x, x')));
      assert (fix (OneBranch(x, y)) = (OneBranch(x, y)));
      assert (fix (OneBranch(x, z)) = (OneBranch(z, x)));
      assert (fix (TwoBranch(Odd, x, Leaf x', Leaf x')) 
        = (TwoBranch(Odd, x, Leaf x', Leaf x')));
      assert (fix (TwoBranch(Even, x, Leaf z, Leaf z')) 
        = (TwoBranch(Even, z, Leaf x, Leaf z')));
      assert (fix (TwoBranch(Even, x, OneBranch(z, x'), OneBranch(z, x'))) 
        = (TwoBranch(Even, z, OneBranch(x, x'), OneBranch(z, x'))));
      assert (fix (TwoBranch(Odd, x, Leaf y, Leaf y')) 
        = (TwoBranch(Odd, x, Leaf y, Leaf y')));
      assert (fix (TwoBranch(Even, x, Leaf x', Leaf y)) 
        = (TwoBranch(Even, x, Leaf x', Leaf y)));
      assert (fix (TwoBranch(Odd, x, Leaf z, Leaf x')) 
        = (TwoBranch(Odd, z, Leaf x, Leaf x')));
      assert (fix (TwoBranch(Odd, x, OneBranch(z, z'), Leaf x')) 
        = (TwoBranch(Odd, z, OneBranch(z', x), Leaf x')));
      assert (fix (TwoBranch(Even, y, Leaf x, Leaf y)) 
        = (TwoBranch(Even, x, Leaf y, Leaf y)));
      assert (fix (TwoBranch(Odd, x, Leaf x', Leaf z)) 
        = (TwoBranch(Odd, z, Leaf x', Leaf x)));
      assert (fix (TwoBranch(Odd,y, Leaf y', TwoBranch(Even,x,Leaf z,Leaf z'))) 
        = (TwoBranch(Odd, x, Leaf y', TwoBranch(Even, z, Leaf y, Leaf z'))));
      assert (fix (TwoBranch(Even, x, Leaf y, Leaf x')) 
        = (TwoBranch(Even, x, Leaf y, Leaf x')));
      assert (fix (TwoBranch(Odd, z, Leaf y, Leaf x)) 
        = (TwoBranch(Odd, z, Leaf y, Leaf x)));        
      assert (fix (TwoBranch (Even, y, OneBranch(x, y), OneBranch(z, z))) 
        = TwoBranch(Even, z, OneBranch(x, y), OneBranch(z, y)));
      assert (fix (TwoBranch(Odd, x, OneBranch(z, y), Leaf y)) 
        = TwoBranch(Odd, z, OneBranch(x, y), Leaf y))

    let extract_tree (q : queue) : tree =
      match q with
      | Empty -> raise QueueEmpty
      | Tree t -> t

    (*..................................................................
    Takes a tree, and returns the item that was most recently inserted
    into that tree, as well as the queue that results from removing
    that element.  Notice that a queue is returned.  (This happens
    because removing an element from just a leaf would result in an
    empty case, which is captured by the queue type).

    By "item most recently inserted", we don't mean the most recently
    inserted *value*, but rather the newest node that was added to the
    bottom-level of the tree. If you follow the implementation of add
    carefully, you'll see that the newest value may end up somewhere
    in the middle of the tree, but there is always *some* value
    brought down into a new node at the bottom of the tree. *This* is
    the node that we want you to return.
    ..................................................................*)
    let rec get_last (t : tree) : elt * queue =    
      match t with 
      | Leaf e1 -> e1, Empty
      | OneBranch(e1, e2) -> e2, Tree (Leaf e1)
      | TwoBranch(Even, e1, t1, t2) -> 
          let last, q2' = get_last t2 in
          (match q2' with
          | Empty -> last, Tree (OneBranch (e1, get_top t1))
          | Tree t2' -> last, Tree (TwoBranch(Odd, e1, t1, t2')))
      (* Since the resulting queue of get_last can't be Empty in the
         TwoBranch odd case, a match statement isn't required *)
      | TwoBranch(Odd, e1, t1, t2) -> 
          let last, q1' = get_last t1 in
          last, Tree (TwoBranch(Even, e1, extract_tree q1', t2))

    let test_get_last () = 
      let x = C.generate () in
      let y = C.generate () in
      let z = C.generate () in
      assert (get_last (Leaf x) = (x, Empty));
      assert (get_last (OneBranch(x, y)) = (y, Tree (Leaf x)));
      assert (get_last (TwoBranch(Even, x, Leaf y, Leaf z)) 
        = (z, Tree(OneBranch(x, y))));
      assert (get_last (TwoBranch(Even, x, Leaf y, OneBranch(z, z))) 
        = (z, Tree(TwoBranch(Odd, x, Leaf y, Leaf z))));
      assert (get_last (TwoBranch(Odd, x, OneBranch(z, z), Leaf y)) 
        = (z, Tree(TwoBranch(Even, x, Leaf y, Leaf z))));
      assert (get_last (TwoBranch(Odd, x, 
                       TwoBranch(Even, x, Leaf y, Leaf z), Leaf y)) 
        = (z, Tree(TwoBranch(Even, x, OneBranch(x, y), Leaf y))));
      assert (get_last (TwoBranch(Even, x, 
                       TwoBranch(Even, x, Leaf y, Leaf z), Leaf y)) 
        = (y, Tree(OneBranch(x, x))));
      assert (get_last (TwoBranch(Odd, x, 
                       OneBranch(y, z), Leaf y)) 
        = (z, Tree(TwoBranch(Even, x, Leaf y, Leaf y))))

    (*..................................................................
    Implements the algorithm described in the writeup. You must finish
    this implementation, as well as the implementations of get_last
    and fix, which take uses.
    ..................................................................*)
    let take (q : queue) : elt * queue =
      match extract_tree q with
      (* If the tree is just a Leaf, then return the value of that leaf, and the
       * new queue is now empty *)
      | Leaf e -> e, Empty

      (* If the tree is a OneBranch, then the new queue is just a Leaf *)
      | OneBranch (e1, e2) -> e1, Tree (Leaf e2)

      (* Removing an item from an even tree results in an odd
         tree. This implementation replaces the root node with the
         most recently inserted item, and then fixes the tree that
         results if it is violating the strong invariant *)
      | TwoBranch (Even, e, t1, t2) ->
         let last, q2' = get_last t2 in
         (match q2' with
          (* If one branch of the tree was just a leaf, we now have
             just a OneBranch *)
          | Empty -> e, Tree (fix (OneBranch (last, get_top t1)))
          | Tree t2' -> e, Tree (fix (TwoBranch (Odd, last, t1, t2'))))
      (* Implement the odd case! *)
      | TwoBranch (Odd, e, t1, t2) ->
         let last, q1' = get_last t1 in
         e, Tree (fix (TwoBranch (Even, last, extract_tree q1', t2)))

    let rec size (t: tree) : bool =
      let rec subtree_size subtree =
        let _, q' = get_last subtree in 
        match q' with
        | Empty -> 1
        | Tree t' -> 1 + subtree_size t'
      in
      match t with
      | Leaf _ -> true
      | OneBranch(_, _) -> true
      | TwoBranch(Odd, _, t1, t2) -> 
          subtree_size t1 = (subtree_size t2 + 1) && size t1 && size t2
      | TwoBranch(Even, _, t1, t2) -> 
          subtree_size t1 = subtree_size t2 && size t1 && size t2

    let test_take () =
      let x = C.generate () in
      let x2 = C.generate_gt x in
      let x3 = C.generate_gt x2 in
      let x4 = C.generate_gt x3 in
      assert (take (add x3 empty) = (x3, empty));
      assert (take (add x3 (add x2 empty)) 
        = (x2, (add x3 empty)));
      let _, q = take (add x3 (add x2 empty)) in
      assert (size (extract_tree q));
      assert (take (add x3 (add x2 (add x empty))) 
        = (x, (add x3 (add x2 empty))));
      let _, q = take (add x3 (add x2 (add x empty))) in
      assert (size (extract_tree q));
      assert (take (add x2 (add x (add x3 empty))) 
        = (x, add x3 (add x2 empty))); 
      assert (take (Tree (TwoBranch(Odd, x, 
                       OneBranch(x2, x3), Leaf x4))) 
        = (x, Tree (TwoBranch (Even, x2, Leaf x3, Leaf x4))));
      assert (take (Tree (TwoBranch(Odd, x, TwoBranch(Even, x2, Leaf x4, 
                            Leaf x3), OneBranch(x2, x4))))
        = (x, Tree (TwoBranch (Even, x2, OneBranch(x3, x4), 
                      OneBranch(x2, x4)))))

    let run_tests () = 
      test_get_top ();
      test_fix ();
      test_get_last ();
      test_take ();
      ()

  end

(*......................................................................
Now to actually use the priority queue implementations for something
useful!

Priority queues are very closely related to sorts. Remember that
removal of elements from priority queues removes elements in highest
priority to lowest priority order. So, if your priority for an element
is directly related to the value of the element, then you should be
able to come up with a simple way to use a priority queue for
sorting...

In OCaml 3.12 and above, modules can be turned into first-class
values, and so can be passed to functions! Here, we're using that to
avoid having to create a functor for sort. Creating the appropriate
functor is a challenge problem :-)

The following code is simply using the functors and passing in a
COMPARABLE module for integers, resulting in priority queues tailored
for ints.
......................................................................  *)

module IntListQueue = (ListQueue(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)

module IntHeapQueue = (BinaryHeap(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)

module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* Store the whole modules in these variables *)
let list_module = (module IntListQueue :
                     PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue :
                     PRIOQUEUE with type elt = IntCompare.t)

let tree_module = (module IntTreeQueue :
                     PRIOQUEUE with type elt = IntCompare.t)

(* Implementing sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in
  let rec extractor pq lst =
    if P.is_empty pq then lst
    else
      let (x, pq') = P.take pq in
      extractor pq' (x::lst) in
  let pq = List.fold_right P.add lst P.empty in
  List.rev (extractor pq [])


(* Now, we can pass in the modules into sort and get out different
   sorts. *)

(* Sorting with a priority queue with an underlying heap
   implementation is equivalent to heap sort! *)
let heapsort = sort heap_module ;;

(* Sorting with a priority queue with your underlying tree
   implementation is *almost* equivalent to treesort; a real treesort
   relies on self-balancing binary search trees *)

let treesort = sort tree_module ;;

(* Sorting with a priority queue with an underlying unordered list
   implementation is equivalent to selection sort! If your
   implementation of ListQueue used ordered lists, then this is really
   insertion sort. *)
let selectionsort = sort list_module

(* You should test that these sorts all correctly work, and that lists
   are returned in non-decreasing order. *)

(*......................................................................
Section 4: Challenge problem: Sort function

A reminder: Challenge problems are for your karmic edification
only. You should feel free to do these after you've done your best
work on the primary part of the problem set.

Above, we only allow for sorting on int lists. Write a functor that
will take a COMPARABLE module as an argument, and allows for sorting
on the type defined by that module. You should use your BinaryHeap
module.

As challenge problems go, this one is relatively easy, but you should
still only attempt this once you are completely satisfied with the
primary part of the problem set.
......................................................................*)

(*......................................................................
Section 5: Challenge problem: Benchmarking

Now that you are learning about asymptotic complexity, try to write
some functions to analyze the running time of the three different
sorts. Record in a comment here the results of running each type of
sort on lists of various sizes (you may find it useful to make a
function to generate large lists).  Of course include your code for
how you performed the measurements below.  Be convincing when
establishing the algorithmic complexity of each sort.  See the CS51
and Sys modules for functions related to keeping track of time
......................................................................*)


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)

let minutes_spent_on_part () : int = 720 ;;
