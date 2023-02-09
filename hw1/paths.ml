type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree

let rec cat (l : 'a list) (m : 'a list): 'a list =
  match l, m with
  | [] , _ -> m
  | _ , [] -> l
  | hl::tl , hm::tm -> 
      hl::(cat tl m)
  
let rec nodeAddHelper (l: 'a list list) (x : 'a): 'a list list = 
  match l with
  | [] -> []
  | [[]] -> [[]]
  | hd::tl -> cat ([cat [x] hd]) (nodeAddHelper tl x)

let rec paths (t: 'a tree) : 'a list list =
  match t with
  | Leaf(x) -> [[x]]
  | Node (x, l, r) -> let l' = cat (paths l) (paths r) in
      match l' with
      | [] -> []
      | [[]] -> [[]]
      | _ -> nodeAddHelper l' x 
