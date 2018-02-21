(*
                         CS 51 Problem Set 4
                Modules, Functors, and Priority Queues
                           Priority Queues
                             Spring 2018
*)

open Order

open Orderedcoll ;;

module type PRIOQUEUE =
sig
  exception QueueEmpty

  type elt
  type queue

  val empty : queue
  val is_empty : queue -> bool
  val add : elt -> queue -> queue
  val take : queue -> elt * queue
  val to_string : queue -> string
  val run_tests : unit -> unit
end

module ListQueue(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)
module TreeQueue (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)
module BinaryHeap (C : COMPARABLE) : (PRIOQUEUE with type elt = C.t)

module IntHeapQueue : (PRIOQUEUE with type elt = IntCompare.t)

val minutes_spent_on_part : unit -> int