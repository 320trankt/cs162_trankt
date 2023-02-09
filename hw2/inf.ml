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
  Cons (fun () -> (0, 1, fib ))

let rec firstn n xs =
  failwith "Your code here"

let rec interleave xs ys =
  Cons (fun () -> match xs, ys with
      | xsh::xst, ysh::yst -> (xsh, interleave ys xst))


let z =
  failwith "Your code here"

let product xs ys =
  failwith "Your code here"

let corner n xss =
  failwith "Your code here"

let rec diag xxs =
  failwith "Your code here"