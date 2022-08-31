let rec sum = function
  | [] -> 0
  | h::t -> h + sum t 

let s = sum [1;2;3]

let rec combine f acc = function 
  | [] -> acc 
  | h::t -> f h (combine f acc t)

(* okay, then we can make fold_right function *)
(* you need to understand this function calculates from right element to left element in list *)
let rec fold_right f lst acc = match lst with 
  | [] -> acc
  | h::t -> f h (fold_right f t acc)
  
(* Digression *)
(* labeled fold functions *)
let rec fold_left ~op:(f: acc:'a -> elt:'b -> 'a) ~init:acc lst = 
  match lst with df
  | [] -> acc 
  | h::t -> t -> fold_left ~op:f ~init:(f ~acc:acc ~elt:h) t

let length lst = List.fold_left (fun acc _ -> acc + 1) 0 lst 