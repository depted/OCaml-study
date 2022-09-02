(*
  val mem : 'a -> 'a t -> bool 
include is used for principled copy-and-paste   
*)

module type Set = sig 
  type 'a t 
  val empty : 'a t 
  val mem : 'a -> 'a t -> bool 
  val add : 'a -> 'a t -> 'a t 
  val elements : 'a t - 'a list 
end

module ListSet : Set = struct 
  type 'a t = 'a list 
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s 
end

(* OCaml includes are similar to inheritance. They enable a module to include all the items defined by another module, or a module type to include all the specifications of another module type.*)
(* suppose we wanted to add a function of_list : 'a list -> 'a t *)

(*Here is how we can use includes to solve the problem of adding of_list to ListSet *)
module ListSetExtended = struct 
  include ListSet
  let of_list lst = List.fold_right add lst empty 
end
(* "include" includes all definitions of target module *)

module type SetExtended = sig 
  type 'a t 
  val empty : 'a t 
  val mem : 'a -> 'a t -> bool 
  val add : 'a -> 'a t -> 'a t
  val elements : 'a t -> 'a list 
  val of_list : 'a list -> 'a t 
end 

module ListSetImpl = struct
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let add = List.cons
  let elements s = List.sort_uniq Stdlib.compare s
end

module ListSet : Set = ListSetImpl

module type SetExtended = sig
  include Set
  val of_list : 'a list -> 'a t
end

module ListSetExtendedImpl = struct
  include ListSetImpl
  let of_list lst = function 
    | [] -> []
    | h ::  t -> if mem h t then elements t else h :: elements t 
end

module ListSetExtended : SetExtended = ListSetExtendedImpl

(* articulate difference between include and open*)
module M = struct
  let x = 0
end

module N = struct
  include M
  let y = x + 1
end

module O = struct
  open M
  let y = x + 1
end

(*
include is more powerful than open. include means including every definition of module literally. the one that is included is involved with using module.
open is not including a module. it just makes moudule as 0 in scope.    

A metaphor for understanding this difference might be: open M imports definitions from M and makes them available for local consumption, but they aren’t exported to the outside world. Whereas include M imports definitions from M, makes them available for local consumption, and additionally exports them to the outside world.
*)
