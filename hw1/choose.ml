let rec cat (l : 'a list) (m : 'a list): 'a list =
  match l, m with
  | [] , _ -> m
  | _ , [] -> l
  | hl::tl , hm::tm -> 
      hl::(cat tl m)
          
let rec listSize (l : 'a list) : int = 
  match l with
  | [] -> 0
  | h::t -> 1 + listSize t
              
let rec nodeAddHelper (l: 'a list list) (x : 'a): 'a list list = 
  match l with
  | [] -> [[x]]
  | [[]] -> [[x]]
  | hd::[] -> [cat [x] hd]
  | hd::tl -> cat ([cat [x] hd]) (nodeAddHelper tl x) 
                
let rec choose (l : 'a list) (n : int) : 'a list list = 
  match n with 
  | 0 -> [[]]
  | x -> if x > listSize l then [[]] else
      if x = listSize l then [l] else match l with
        | [] -> []
        | h::t -> cat (nodeAddHelper (choose t (n-1)) (h)) (choose t n)
