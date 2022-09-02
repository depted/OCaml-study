(*
functor is map between categories. 
Likewise, OCaml modules contain functions, and OCaml functors map from modules to modules.   
*)

(* Here’s a tiny example of a functor: *)
module type X = sig
  val x : int
end

module IncX (M : X) = struct 
  let x = M.x = 1 
end

(* The functor name is IncX and the parameter is M
parameterized structure   
*)

(*
  syntax of functor:

  module F (M : S) = ...
  end   

  like function, functor also has synonyn like,

  module F (M : S) = ...
  <=>
  module F = functor (M : S) -> ...
*)

(*
  Functor types are an example of an advanced programming language feature called dependent types, 
  with which the type of an output is determined by the value of an input. 
*)

module type OrderedType = sig 
  type t 
  val compare : t -> t -> int 
end

