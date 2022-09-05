(* Function Documentation *)

(* in this part, we will see how to define spec.*)

(* return clause means return value. it is needed to be written as a specification of function *)
(* for example,
     (** returns: [sqr x] is the square root of [x]. *)
*)

(* 6.2.2 requires clause *)
(* we need to limit a range to be applied to sqrt *)
(** [sqr x] is the square root of [x]. Its relative accuracy is no worse
    than [1.0e-6].  Requires: [x >= 0] *)

6.2.3 Raises Clause 

A better way to make functions total is to have them raise an exception when the expected input condition is not met. Exceptions avoid the necessity of distracting error-handling logic in the client’s code. If the function is to be total, the specification must say what exception is raised and when. For example, we might make our square root function total as follows:
Remember, dont try to if-else to handle something not involved with domain, you might as well use exception!

6.2.5 The specification game

When you make a function, you gotta think about the spec is 'iff' with the function itself.