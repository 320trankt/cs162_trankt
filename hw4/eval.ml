open Ast

(** Variable set. Based on OCaml standard library Set. *)
module VarSet = Set.Make (String)

(* Helper function for parsing an expression. Useful for testing. *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)
(*******************************************************************|
|**********************   Interpreter   ****************************|
|*******************************************************************|
|*******************************************************************)

(* Exception indicating that evaluation is stuck *)
exception Stuck of string

(* Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(* Raises an exception for things that need to be implemented
 * in this assignment *)
let todo () = failwith "TODO"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : VarSet.t =
  match e with
  | Var x -> VarSet.singleton(x)
  | Lambda (x, e) -> VarSet.remove(x, free_vars e)
  | App (e1, e2) -> VarSet.union(free_vars e1, free_vars e2)
  | LetBind(x, e1, e2) -> VarSet.remove(x, free_vars e2)
  | NumLit n -> VarSet.empty
  | Binop (e1, op, e2) -> VarSet.union(free_vars e1, free_vars e2)
  | IfThenElse (e1, e2, e3) -> VarSet.union(VarSet.union(free_vars e1, free_vars e2), free_vars e3)
  | _ -> im_stuck "Free_vars error"

(** Performs the substitution [x -> e1]e2 *)
let rec subst (x : string) (e1 : expr) (e2 : expr) : expr =
  match e2 with
  | Var s -> if s = Var(x) then Var(x) else s
  | Lambda(x, e) -> Lambda(x, subst x e1 e)

(** Evaluates e. You need to copy over your
   implementation of homework 3. *)
   let rec eval (e : expr) : expr =
    try
      match e with
      (* Things you need to implement *)
      (* Values *)
      | NumLit n -> NumLit n
      | Lambda (x, e) -> Lambda (x, e)
      | ListNil -> ListNil
      | ListCons (e1, e2) -> ListCons(eval e1, eval e2)
      (* Expressions *)
      (* HW3 *)
      | Binop (e1, op, e2) -> (match op with
        | Add -> ( match eval e1, eval e2 with
          | NumLit 0, NumLit 0 -> eval (NumLit 0)
          | NumLit 0, NumLit x -> eval e2
          | NumLit x, NumLit 0 -> eval e1
          | NumLit x, NumLit y -> eval (NumLit (x + y))
          | NumLit x, _ -> im_stuck "Addition failure"
          | _, NumLit y -> im_stuck "Addition failure"
          | _, _ -> im_stuck "Addition failure"
        )
        | Sub -> (match eval e1, eval e2 with
          | NumLit 0, NumLit 0 -> eval (NumLit 0)
          | NumLit 0, NumLit x -> eval e2
          | NumLit x, NumLit 0 -> eval e1
          | NumLit x, NumLit y -> eval (NumLit (x - y))
          | NumLit x, _ -> im_stuck "Subtraction failure"
          | _, NumLit y -> im_stuck "Subtraction failure"
          | _, _ -> im_stuck "Subtraction failure"
        )
        | Mul -> (match eval e1, eval e2 with
          | NumLit 0, NumLit x -> eval (NumLit 0)
          | NumLit x, NumLit 0 -> eval (NumLit 0)
          | NumLit 1, NumLit x -> eval e2
          | NumLit x, NumLit 1 -> eval e1
          | NumLit x, NumLit y -> eval (NumLit (x*y))
          | NumLit x, _ -> im_stuck "Multiplication failure"
          | _, NumLit y -> im_stuck "Multiplication failure"
          | _, _ -> im_stuck "Multiplication failure"
        )
        | Gt -> (match eval e1, eval e2 with
          | NumLit x, NumLit y -> if x > y then NumLit 1 else NumLit 0
          | _, _ -> im_stuck "Greater Than Error"
        )
        | Lt -> (match eval e1, eval e2 with
          | NumLit x, NumLit y -> if x < y then NumLit 1 else NumLit 0
          | _, _ -> im_stuck "Less Than Error"
        )
        | And -> (match eval e1, eval e2 with
          | NumLit x, NumLit y -> if (x <> 0 && y <> 0) then NumLit 1 else NumLit 0
          | _, _ -> im_stuck "And Error"
        )
        | Or -> (match eval e1, eval e2 with
          | NumLit x, NumLit y -> if (x <> 0 || y <> 0) then NumLit 1 else NumLit 0
          | _, _ -> im_stuck "Or Error"
        )
        | Eq -> (match eval e1, eval e2 with
          | NumLit x, NumLit y -> if x = y then NumLit 1 else NumLit 0
          | _, _ -> im_stuck "Equality Error"
        )
      )
      | IfThenElse (e1, e2, e3) -> (match eval e1 with
        | NumLit x -> if x <> 0 then eval e2 else eval e3
        | _ -> im_stuck "IfThenElse error"
      )
      | ListCons (e1, e2) -> ListCons(eval e1, eval e2)
      | ListHead e -> (match eval e with
        | ListCons (h, t) -> eval h
        | _ -> im_stuck "ListHead error"
      )
      | ListTail e -> (match eval e with
          | ListCons(h, t) -> eval t
          | _ -> im_stuck "ListTail error"
      )
      | ListIsNil e -> (match eval e with
        | ListNil -> NumLit 1
        | ListCons(h, t) -> NumLit 0
        | _ -> im_stuck "ListIsNil error"
      )
      (* HW4 *)
      | Var s -> im_stuck "Free Variable Error"
      | App (e1, e2) -> (match eval e1 with
        | Lambda (x, e1) -> subst x e2 e1
        | _ -> im_stuck "App Error"

      )
    with
    | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)
  ;;