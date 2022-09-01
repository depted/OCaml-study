(* this codes are written based on cs3110 in cornell univ.*)

(*motivating example about violating encapsulation *)
(* a moudle to support usual addition and multiplication operations from arithmetic, so-called ring! *)

module type Ring = sig 
  type t 
  val zero : t 
  val one : t 
  val ( + ) : t -> t -> t 
  val ( * ) : t -> t -> t 
  val ( ~- ) : t -> t (* additive inverse *)
  val to_string : t -> string 
end 

module IntRing : Ring = struct 
  type t = int 
  let zero = 0 
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int 
  let pp_intring fmt i = Format.fprintf fmt "%s" (IntRing.to_string i );;

end 

(* this is float version *)
module FloatRing : Ring = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = Stdlib.( +. )
  let ( * ) = Stdlib.( *. )
  let ( ~- ) = Stdlib.( ~-. )
  let to_string = string_of_float
end

(* using type constraint! *)
module type INT_RING = Ring with type t = int
module type Float_ring = Ring with type t = float 

(* 5.7.2 constraints! *)

(*
Syntax.

There are two sorts of constraints. One is the sort we saw above, with type equations:
T with type x = t, where T is a module type, x is a type name, and t is a type.
The other sort is a module equation, which is syntactic sugar for specifying the equality of all types in the two modules:
T with module M = N, where M and N are module names.
Multiple constraints can be added with the and keyword:
T with constraint1 and constraint2 and ... constraintN   
*)
