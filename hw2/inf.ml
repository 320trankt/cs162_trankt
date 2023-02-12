type 'a inf = Cons of (unit -> ('a * 'a inf))

let hd xs = 
  match xs with
  | Cons f -> fst (f ())

let tl xs = 
  match xs with
  | Cons f -> snd (f ())

let cons x xs =
  Cons (fun () -> (x, xs))

let rec from n =
  Cons (fun () -> (n, from (n+1)))

let rec map f xs =
  Cons (fun () -> f (hd xs), map f (tl xs))

let rec repeat x = 
  Cons (fun () -> (x, repeat x))
                      
let fib =  
  let rec fibHelper (n : int) (m : int) = Cons (fun () -> (n, (fibHelper m (n+m)) ))
  in fibHelper 0 1

let rec firstn n xs =
  match n, xs with
  | 0, l -> []
  | x, l -> (hd l)::(firstn (x-1) (tl l))

let rec interleave xs ys =
  Cons (fun () -> (hd xs, interleave ys (tl xs)))

let fail_inf = Cons (fun () -> failwith "sth")

let z = 
  let listNat = (from 1) in 
    let listNeg = map (fun (x : 'a) -> -1 * x) listNat in cons 0 (interleave listNat listNeg)

let product xs ys =
  failwith "Your code here"

let corner n xss =
  failwith "Your code here"

let rec diag xxs =
  failwith "Your code here"