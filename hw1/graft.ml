type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec graft (t: 'a tree) (t': 'a tree) : 'a tree = 
  match t with
  | Leaf(x) -> t'
  | Node (x, l, r) -> Node (x, graft l t', graft r t')