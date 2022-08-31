type 'a tree = 
  | Leaf 
  | Node of 'a * 'a tree * 'a tree

let rec map_tree f = function 
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map_tree f l, map_tree f r)

let rec fold_tree f acc = function 
  | Leaf -> acc 
  | Node (v, l, r) -> f v (fold_tree f acc l) (fold_tree f acc r)

let size t = fold_tree (fun _ l r -> 1 + l + r) 0 t 
let depth t = fold_tree (fun _ l r -> 1 + max l r) 0 t 
let preorder t = fold_tree (fun x l r -> [x] @ l @ r) [] t 

(*catamorphism, aka a generalized fold operation. To learn more about catamorphisms, take a course on category theory.*)