module MyModule = struct 
  let inc x = x + 1
  type primary_color = Red | Green | Blue
  exception Oops
end

module ListStack = struct 
  let empty = []
  let is_empty = function [] -> true | _ -> false
  let push x s = x :: s

  exception Empty

  let peek = function 
    | [] -> raise Empty 
    | x::_ -> x 

  let pop = function 
    | [] -> raise Empty
    | _ :: s -> s 
end 

ListStack.(empty |> push 1 |> push 2)

(*
let binds a value to a name, whereas
module binds a module value to a name.
*)

(* remove surrounding whitespace from [s] and convert it to lower case *)
let s = "BigRed "
let s' = s |> String.trim |> String.lowercase_ascii
let s'' = String.(s |> trim |> lowercase_ascii)

(* What if you wanna bring a module into scipe? The answer is "let open M in e" *)
let lower_trim s = 
  let open String in 
  s |> trim |> lowercase_ascii

module type LIST_STACK = sig
  exception Empty
  val empty : 'a list
  val is_empty : 'a list -> bool
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a
  val pop : 'a list -> 'a list
end

module ListStack : LIST_STACK = struct
  let empty = []
  let is_empty = function [] -> true | _ -> false 
  let push x s = x :: s
  exception Empty 
  let peek = function 
    | [] -> raise Empty
    | x::_ -> x
  let pop = function 
    | [] -> raise Empty
    | _::s -> s
end 

(* let's alias example *)
module ListStackAlias : LIST_STACK = ListStack
(* equivalently *)
module ListStackAlias = (ListStack : LIST_STACK)

module type X = sig
  val x : int 
end

module type T = sig 
  module Inner : X 
end

module M : T = struct 
  module Inner : X = struct 
    let x = 42
  end
end

(* if moudule uses smoething not in module type, then it's okay *)
(* if moudule doesnt use smoething in module type, then it isn't acceptable *)