(* introduction of functor!*)
(*
In category theory, a category contains morphisms, which are a generalization of functions as we known them, and a functor is map between categories. Likewise, OCaml modules contain functions, and OCaml functors map from modules to modules.   
*)

module type X = sig 
  val x : int 
end

module IncX (M : X) =  struct 
  let x = M.x + 1 
end 

module A = struct let x = 0 end

A.x 

module B = IncX (A)

B.x

module AddX (M : X) = struct 
  let add y = M.x + y 
end 

module Add42 = AddX (struct let x = 42 end)

(*
the type annotation : S and the parentheses around it, (M : S) are required. The reason why is that OCaml needs the type information about S to be provided in order to do a good job with type inference for F itself.   

module F (M : S) = ...

module F = functor (M : S) -> ...

The second form uses the functor keyword to create an anonymous functor, like how the fun keyword creates an anonymous function.
*)

module type Add = sig val add : int -> int end 
module CheckAddX : X -> Add = AddX

module type T = sig 
  type t 
  val x : t
end 

module Pair1 (M : T) = struct 
  let p = (M.x, 1)
end 

(* so, pair1 could be written in another way like as follows: *)
module type P1 = functor (M : T) -> sig val p : M.t * int end

module Pair1 : P1 = functor (M : T) -> struct
  let p = (M.x, 1)
end

module F (M : sig val x : int end) = struct let y = M.x end 
module X = struct let x = 0 end 
module Z = struct let x = 0;; let z = 0 end 
module FX = F (X)
module FZ = F (Z)

module type OrderedType = sig 
  type t 
  val compare : t -> t -> int 
end

module type S = sig 
  type key 
  type 'a t 
  val empty: 'a t 
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t 
  val find: key -> 'a t -> 'a 
  ...
end 

module IntMap = Map.Make(Int)

module Make (Ord : OrderedType) = struct 
  type key = Ord.t 

  type 'a t = 
    | Empty 
    | Node of {l : 'a t; v : key; d: 'a; r : 'a t; h : int}

  let empty = Empty 

  let rec mem x = function 
    | Empty -> false 
    | Node{l, v, r} ->
      let c = Ord.compare x v in 
      c = 0 || mem x (if c < 0 then l else r)
    
    ...
end

(*  Maps with Custom key types*)
type name = {first : string; last: string}

module Name = struct 
  type t = name 
  let compare {first = first1; last = last1} {first = first2; last = last2}
    = 
    match String.compare last1 last with 
    | 0 -> String.compare first1 first2
    | c -> c 
end 

(* How Map use module type constraints*)

(* In the standard library’s map.mli interface, the specification for Map.Make is: *)

module Make (Ord : OrderedType) : S with type key = Ord.t

(* 5.9.4 Using Functors! *)
exception Empty

module type Stack = sig 
  type 'a t 
  val empty : 'a t 
  val push : 'a -> 'a t -> 'a t 
  val peek : 'a t -> 'a 
  val pop : 'a t -> 'a t
end

module ListStack = struct
  type 'a t = 'a list 
  let empty = []
  let push = List.cons
  let peek = function [] -> raise Empty | x::_ -> x 
  let pop = function [] -> raise Empty | _::s -> s

module VariantStack = struct
  type 'a t = E | S of 'a * 'a t 
  let empty = E 
  let push x s = S (x, s)
  let peek = function E -> raise Empty | S (x, _) -> x 
  let pop = function E -> raise Empty | S (_, s) -> s  
end 

let test = "peek (push x empty) = x" >:: fun _ -> 
  assert_equal 1 ListStack.(empty |> push 1 |> peek)

let test' = "peek (push x empty) = x" >:: fun _ ->
  assert_equal 1 VariantStack.(empty |> push 1 |> peek)

module StackTester (S: Stack) = struct 
  let tests = [
    "peek (push x empty) = x" >:: fun _ -> 
      assert_equal 1 S.(empty |> push 1 |> peek)
  ]
end

module ListStackTester = StackTester (ListStack)
module VariantStackTester = StackTester (VariantStack)

let all_tests = List.flatten [
  ListStackTester.tests;
  VariantStackTest.tests
]

let stacks = [ (module ListStack : Stack); (module VariantStack)]

let all_tests = 
  let tests m = 
    let module S = (val m : Stack) in 
    let module T = StackTester (S) in 
    T.tests
  in
  let open List in 
  stacks |> map tests |> faltten 

module type Set = sig 
  type 'a t 
  val empty : 'a t 
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t 
  val elements : 'a t -> 'a list
end

module SefOfList (S: Set) = struct 
  let of_list lst = List.fold_right S.add lst S.empty 
end

module ListSet : Set = struct
  type 'a t = 'a list 
  let empty = []
  let mem = List.mem
  let add = List.cons 
  let elements s = List.sort_uniq Stdlib.compare s 
end

module UniqListSet : Set = struct 
  type 'a t = 'a list 
  let empty = []
  let mem = List.mem 
  let add x s = if mem x s then s else x :: s 
  let elements = Fun.id 
end 

module OfList = SetOfList(ListSet)
module UniqOfList = SetOfList (UniqListSet)

module SetWithOfList (S : Set) = struct 
  include S
  let of_lst lst = List.fold_right S.add lst S.empty 
end 
