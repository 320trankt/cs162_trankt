let rec cat (l : 'a list) (m : 'a list): 'a list =
match l, m with
| [] , _ -> m
| _ , [] -> l
| hl::tl , hm::tm -> 
    hl::(cat tl m)