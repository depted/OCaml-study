(* tried to hide fact_aux however, it would be disclosed *)
module Math = struct 
  let rec fact_aux n acc = 
    if n = 0 then acc else fact_aux (n-1) (n * acc)

  (** [fact n] is [n!]. *)
  let fact n = fact_aux n 1 
end

(* this one doesnt show fact_aux. it means, nesting makes code safe again *)
module Math = struct 
  let fact n =
    let rec fact_aux n acc = 
      if n = 0 then acc else fact_aux (n-1) (n * acc)
    in
    fact_aux n 1
end 

(* another method to show something that you wanna show only. *)
(* module type makes any components not named in the module type be inaccessible.*)
module type MATH = sig s
  val fact : int -> int 
end

module Math : MATH = struct 
  let rec fact_aux n acc = 
    if n = 0 then acc else fact_aux (n - 1) (n * acc)
  let fact n = fact_aux n 1 
end

(* if we think about those concepts based on java, those're like...*)
module type C_PUBLIC = sig
  val y : int
end

module CPrivate = struct
  let x = 0
  let y = 0
end

module C : C_PUBLIC = CPrivate

(* we made stack using list in cp 4 like this! *)
module type LIST_STACK = sig
  (** [Empty] is raised when an operation cannot be applied
      to an empty stack. *)
  exception Empty

  (** [empty] is the empty stack. *)
  val empty : 'a list

  (** [is_empty s] is whether [s] is empty. *)
  val is_empty : 'a list -> bool

  (** [push x s] pushes [x] onto the top of [s]. *)
  val push : 'a -> 'a list -> 'a list

  (** [peek s] is the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val peek : 'a list -> 'a

  (** [pop s] is all but the top element of [s].
      Raises [Empty] if [s] is empty. *)
  val pop : 'a list -> 'a list
  val size : 'a list -> int
end

module ListStack : LIST_STACK = struct
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length 
end

(* 
  former version needs linear time to know length of stack
  if we wanna take it faster, we need to add cache for saving current length!   
*)
module ListStackCachedSize = struct
  exception Empty 
  let empty = ([], 0)
  let is_empty = function ([], _) -> true | _ -> false 
  let push x (stack, size) = (x :: stack, size + 1)
  let peek = function ([], _) -> raise Empty | (x :: _, _) -> x
  let pop = function 
    | ([], _) -> raise Empty
    | (_::stack,size) -> (stack, size - 1)

(* however, the moudle is not compatible with module type so, we need to use type. *)
(* we need abstract data type. *)
module type LIST_STACK = sig
  type 'a stack (* this one is core*)
  exception Empty
  val empty : 'a stack
  val is_empty : 'a stack -> bool
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
  val size : 'a stack -> int
end

module ListStackCachedSize : LIST_STACK = struct
  type 'a stack = 'a list * int (* this one is core *)
  exception Empty
  let empty = ([], 0)
  let is_empty = function ([], _) -> true | _ -> false
  let push x (stack, size) = (x :: stack, size + 1)
  let peek = function ([], _) -> raise Empty | (x :: _, _) -> x
  let pop = function
    | ([], _) -> raise Empty
    | (_ :: stack, size) -> (stack, size - 1)
  let size = snd
end

(* 5.4.3 pretty printing *)
(* Recall that the toplevel uses this angle-bracket convention to indicate an unprintable value. We’ve encountered that before with functions and <fun>:*)