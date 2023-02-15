open Ast

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

(* Raises an exception for things to be in the next assignment *)
let hw4 () = failwith "Homework 4"

(* Helper function to check that an expression is a value, otherwise raises a
   Stuck exception. *)
let assert_value e =
  if is_value e then () else im_stuck (string_of_expr e ^ " is not a value")

(* Evaluates expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    (* Things you need to implement *)
    | NumLit n -> NumLit n
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
    | ListNil -> ListNil
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
    (* Things you don't need to implement in this assignment *)
    | _ -> hw4 ()
  with
  | Stuck msg -> im_stuck (msg ^ "\nin expression " ^ string_of_expr e)
