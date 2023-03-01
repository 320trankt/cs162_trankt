open Ast

module Env = Map.Make (String) type env = typ Env.t

exception Type_error of string 
let ty_err msg = raise (Type_error msg)

let todo () = failwith "TODO"

let rec typecheck (env : env) (e : expr) : typ =
  try
  match e with
  | NumLit n -> TInt
  | ListNil Some t -> TList t
  | ListCons (e1, e2) -> (match typecheck env e1, typecheck env e2 with
    | t, TList t1 -> TList t1
    | _,_ -> ty_err "Invalid Types for ListCons"
  )
  | Binop (e1, op, e2) -> (match op with
    | Eq -> (match typecheck env e1, typecheck env e2 with
      | TInt, TInt -> TInt
      | TList t1, TList t2 -> TInt
      | _ -> ty_err "Invalid types for equality check")
    | _ -> (match typecheck env e1, typecheck env e2 with
      | TInt, TInt -> TInt
      | _,_ -> ty_err "Invalid types for binary operation"
    )
  )
  | IfThenElse (e1, e2, e3) -> (match typecheck env e1 with
    | TInt -> typecheck env e2 
    | _ -> ty_err "Invalid type for IfThenElse"
  )
  | Var x -> if Env.mem x env then Env.find x env else ty_err "Unbound variable type error"
  | Lambda (x, t, e) -> let env' = Env.add x t env in typecheck env' e
  | App (e1, e2) -> typecheck env e1
  | Fix e -> typecheck env e
  | ListHead e -> (match typecheck env e with
    | TList t -> t
    | _ -> ty_err "Invalid type for listHead"
  )
  | ListTail e -> (match typecheck env e with
    | TList t -> TList t
    | _ -> ty_err "Invalid type for listTail"
  )
  | ListIsNil e -> (match typecheck env e with
    | TList t -> TInt
    | _ -> ty_err "Invalid type for ListIsNil"
  )
  | LetBind (x, t1, e1, e2) -> Map.add x x env 
  | _ -> todo ()
  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)