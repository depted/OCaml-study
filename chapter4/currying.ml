let add' t = fst t + snd take

(* this is how to convert uncurried function into being curried and vice versa. *)
let curry f x y = f (x, y) (* convert in "uncurried"*)
let uncurry f (x, y) = f x y 