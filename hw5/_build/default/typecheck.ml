open Ast

module Env = Map.Make (String) type env = typ Env.t

exception Type_error of string 
let ty_err msg = raise (Type_error msg)

let todo () = failwith "TODO"

let rec typecheck (env : env) (e : expr) : typ =
  try
  match e with
  | NumLit n -> TInt
  | ListNil None -> TList TInt
  | ListCons (e1, e2) -> (match typecheck env e1, typecheck env e2 with
    | TInt, TList t -> TList t
    | TList t1, TList t2 -> TList t2
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
    | TInt -> if eval e1 then typecheck env e2 else typecheck env e3 
    | _ -> ty_err "Invalid type for IfThenElse"
  )
  | Var s -> todo()
  | Lambda (s, x, e) -> todo()
  | App (e1, e2) -> todo()
  | Fix e -> todo()
  | ListHead e -> (match typecheck env e with
    | TList t -> ( match typecheck env t with
      | TInt -> TInt
      | TList t' -> TList t'
      | _ -> ty_err "Invalid type for listHead"
    )
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
  | _ -> todo ()
  with
  | Type_error msg -> ty_err (msg ^ "\nin expression " ^ string_of_expr e)