type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec recursiveHelper (t : 'a tree) (n : int) : int tree * int= 
  match t with
  | Leaf (x)  -> Leaf (n), (n)
  | Node (x, l, r) -> 
      let (leftTree, leftNum) = recursiveHelper l (n+1) in
      let (rightTree, rightNum) = recursiveHelper r (leftNum+1) in 
      Node(n, leftTree, rightTree), rightNum

let rec relabel (t: 'a tree) : int tree = 
  match t with
  | Leaf(x) -> Leaf(0)
  | Node (x, l, r) -> 
      let (leftTree, leftNum) = recursiveHelper l 1 in
      let (rightTree, rightNum) = recursiveHelper r (leftNum + 1) in
      Node (0, leftTree, rightTree)
