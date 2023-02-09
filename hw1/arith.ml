type expr
  = (* A natural number constant *)
    Nat of int
  | (* A variable of unknown value *)
    Var of string
  | (* Addition *)
    Add of expr * expr
  | (* Multiplication *)
    Mul of expr * expr


(* Example: 5 + (1 * 2) *)
let example_expr = Add(Nat(5), Mul(Nat(1), Nat(2)))


let rec simplify (e : expr) : expr =
  match e with
  | Nat x -> Nat x
  | Var x -> Var x
  | Add (x, Nat 0) | Add (Nat 0, x) | Mul (Nat 1, x) | Mul (x, Nat 1) -> simplify x
  | Mul (_, Nat 0) | Mul (Nat 0, _) -> Nat 0
  | Add (Nat x, Nat y) -> Nat (x + y)
  | Mul (Nat x, Nat y) -> Nat (x * y)
  | Add (x, y) -> (match simplify x, simplify y with
      | Nat 0, Nat 0 -> Nat 0
      | Nat 0, _ -> simplify y
      | _, Nat 0 -> simplify x 
      | _, _ -> Add (simplify x, simplify y) 
    )
  | Mul (x, y) -> (match simplify x, simplify y with
      | Nat 0, _ -> Nat 0
      | _, Nat 0 -> Nat 0
      | Nat 1, _ -> simplify y
      | _, Nat 1 -> simplify x
      | _, _ -> Mul (simplify x, simplify y) 
    )