let rec max (l : int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      let recursiveHelper = max t in 
      (match recursiveHelper with
       | None -> Some h
       | Some a -> if h > a then Some h else recursiveHelper
      )