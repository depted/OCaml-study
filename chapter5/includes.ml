(*
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