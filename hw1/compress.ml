let rec cat (l : 'a list) (m : 'a list): 'a list =
match l, m with
| [] , _ -> m
| _ , [] -> l
| hl::tl , hm::tm -> 
    hl::(cat tl m)

let rec compress (l : 'a list) : 'a list = 
match l with
| [] -> []
| h::t -> let helper = compress t in
        (match helper with
        | [] -> [h]
        | x::y -> if h = x then compress helper else h::(compress t)  
        )