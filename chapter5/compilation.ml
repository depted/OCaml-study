(*
  this part explains ml and mli
  let's assume where we got a file called 'x'
  then, x.ml is called the 'implementation' and x.mli is called the 'interface'   
*)

val x : int
val f : int -> int

let x = 0
let y = 12
let f x = x + y

module Foo : sig
  val x : int
  val f : int -> int
end = struct
  let x = 0
  let y = 12
  let f x = x + y
end

module Foo
  : sig (* insert contents of foo.mli here *) end
= struct
  (* insert contents of foo.ml here *)
end

module type Stack = sig 
  type 'a t 
  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool 
  val push : 'a -> 'a t -> 'a t 
  val peek : 'a t -> 'a 
  val pop : 'a t -> 'a t 
  val size : 'a t -> int 
  val to_list : 'a t -> 'a list 
end 

module ListStack : Stack = struct
  type 'a t = 'a list
  exception Empty
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push = List.cons
  let peek = function [] -> raise Empty | x :: _ -> x
  let pop = function [] -> raise Empty | _ :: s -> s
  let size = List.length
  let to_list = Fun.id
end

module type Stack = sig 
  type 'a t 
  val empty : 'a t 
  val is_empty : 'a t -> bool 
  val push : 'a -> 'a t -> 'a t 
  val peek : 'a t -> 'a option 
  val pop : 'a t -> 'a t option 
  val size : 'a t -> int 
  val to_list : 'a t -> 'a list 
end

module ListStack : Stack = struct 
  type 'a t = 'a list 
  exception Empty 
  let empty = []
  let is_empty = function [] -> true | _ -> false 
  let push = List.cons
  let peek = function [] -> None | x::_ -> Some x 
  let pop = function [] -> None | _::s -> Some s
  let size = List.length 
  let to_list = Fun.id 
end

(* fmap and bind in haskell *)
(* useful functions written in infix way *)
(* Option.map aka fmap *)

(* type is 'a option -> ('a -> 'b) -> 'b option *)
let ( >>| ) opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)

(* Option.bind *)
(* type is 'a option -> ('a -> 'b option) -> 'b option *)
let ( >>= ) opt f =
  match opt with
  | None -> None
  | Some x -> f x

module type Stack = sig 
  type 'a t 
  val empty : 'a t 
  val is_empty : 'a t -> bool 
  val push : 'a -> 'a t -> 'a t 
  val peek : 'a t -> 'a 
  val peek_opt : 'a t -> 'a option 
  val pop : 'a t -> 'a t 
  val pop_opt : 'a t -> 'a t option 
  val size : 'a t -> int 
  val to_list : 'a t -> 'a list 
end 

module ListStack : Stack = struct 
  type 'a t = 'a list 
  exception Empty 
  let empty = []
  let is_empty = function [] -> true | _ -> false 
  let push = List.cons 
  let peek = function [] -> raise Empty | x :: _ -> x 
  let peek_opt = function [] -> None | x :: _ -> Some x 
  let pop = function [] -> raise Empty | _ :: s -> s 
  let pop_opt = function [] -> None | _ :: s -> Some s
  let size = List.length 
  let to_list = Fun.id 
end

(* okay, let's make queue module and it's own type *)
module type Queue = sig 
  type 'a t 
  exception Empty 
  val empty : 'a t 
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val front : 'a t -> 'a 
  val dequeue : 'a t -> 'a t 
  val size : 'a t -> int 
  val to_list : 'a t -> 'a list 
end 

module ListQueue : Queue = struct 
  type 'a t = 'a list 
  exception Empty 
  let empty = []
  let is_empty = function [] -> true | _ -> false 
  let enqueue x q = q @ [x]
  let front = function [] -> raise Empty | x :: _ -> x 
  let dequeue = function [] -> raise Empty | _ :: q -> q 
  let size = List.length 
  let to_list = Fun.id 
end 

(* Batched Queue: it is 'efficient' queue implementation *)
module BatchedQueue : Queue = struct 
  type 'a t = { o : 'a list; i : a' list}

  exception Empty 
  
  let empty = {o = []; i = []}

  let is_empty = function 
    | {o = []} -> true 
    | _ -> false 
  
  let enqueue x = function 
    | {o = []} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}

  let front = function 
    | {o = []} -> raise Empty 
    | {o = h :: _} -> h 

  let dequeue = function 
    | {o = []} -> raise Empty 
    | {o = [_]; i} -> {o = List.rev i; i = []} (* the core is this part that should be used as less as possible *)
    | {o = _ :: t; i} -> {o = t; i}

  let size {o; i} = List.(length o + length i)

  let to_list {o; i} = o @ List.rev i 
end 

(* map(aka dictionary) binds keys to value *)
module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  val lookup : 'k -> ('k, 'v) t -> 'v 

  val bindings : ('k, 'v) t -> ('k * 'v) list 
end

module AssocListMap : Map = struct 
  type ('k, 'v) = ('k * 'v) list 
  let empty = []
  let insert k v m = (k, v) :: m 
  let lookup k m = List.assoc k m 
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end 

module type Set = sig 
  type 'a t 
  val empty : 'a t 
  val mem : 'a -> 'a t -> bool 
  val add : 'a -> 'a t -> 'a t 
  val elements : 'a t -> 'a list 
end 

module UniqListSet : Set = struct 
  type 'a t = 'a list 
  let empty = []
  let mem = List.mem
  let add x s = if mem x s then s else x :: s 
  let elements = Fun.id 
end 