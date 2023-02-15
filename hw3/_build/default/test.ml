open Ast
open Eval

(* Unit test utilities *)

let texpr = Alcotest.testable (fun ppf -> fun e -> Fmt.pf ppf "%s" (string_of_expr e)) (=) ;;

(* Helper function to parse an expression *)
let parse (s: string) : Ast.expr =
  Parser.main Scanner.token (Lexing.from_string s)

(* Helper function to check evaluation of an expression *)
let check_eval e expected () =
  let v =
    try eval e with
    | Stuck msg -> failwith ("Got stuck!\n" ^ msg)
  in
    Alcotest.(check' texpr) ~msg:"eval" ~expected:expected ~actual:v

(* Helper function to check evaluation of an expression (given a string of the expression) *)
let check_eval_s s expected () =
  check_eval (parse s) expected ()

(* Helper function to check that evaluation gets stuck *)
let check_stuck s () =
  try
    let v = eval (parse s) in
    Alcotest.fail ("evaluated to " ^ string_of_expr v)
  with
  | Stuck _ -> ()
;;

let open Alcotest in
(* Test that an expression (as a string) evaluates to the expected expression *)
let test_e s exp = test_case s `Quick (check_eval_s s exp) in
(* Test that an expression gets stuck *)
let test_stuck s = test_case s `Quick (check_stuck s) in
(* Test suite *)
run "lambda-plus" [
  "eval", [
    test_e "5" (NumLit(5)) ;
  ];
  "stuck", [
    test_stuck "1 > Nil" ;
  ];
  "Add", [
    test_e "5" (Binop(NumLit(3), Add, NumLit(2)) ) ;
  ];
  "Sub", [
    test_e "5" (Binop(NumLit(7), Sub, NumLit(2))) ;
  ];
  "Mul", [
    test_e "15" (Binop(NumLit(3), Mul, NumLit(5))) ;
  ];
  "Gt", [
    test_e "1" (Binop(NumLit(3), Gt, NumLit(2))) ;
  ];
  "Lt", [
    test_e "1" (Binop(NumLit(3), Lt, NumLit(4))) ;
  ];
  "And", [
    test_e "1" (Binop(Binop(NumLit(3), Gt, NumLit(2)), And, Binop(NumLit(3), Lt, NumLit(4)))) ;
  ];
  "Or", [
    test_e "1" (Binop(Binop(NumLit(1), Gt, NumLit(2)), Or, Binop(NumLit(3), Lt, NumLit(4)))) ;
  ];
  "Eq", [
    test_e "1" (Binop(NumLit(3), Eq, Binop(NumLit(1), Add, NumLit(2)))) ;
  ];
  "IfThenElse", [
    test_e "5" (IfThenElse( Binop(NumLit(3), Eq, Binop(NumLit(1), Add, NumLit(2))) , Binop(NumLit(2), Add, NumLit(3))  , Binop(NumLit(2), Sub, NumLit(3))  )) ;
  ];
  "ListNil", [
    test_e "Nil" (ListNil) ;
  ];
  "ListCons", [
    test_e "Nil@1@2@Nil" (ListCons(ListNil, ListCons(NumLit(1), ListCons(NumLit(2),ListNil)))) ;
  ];
  "ListHead", [
    test_e "5" (ListHead(ListCons(NumLit(5), ListCons(1, ListCons(2, ListNil))))) ;
  ];
  "ListTail", [
    test_e "1@2@Nil" (ListTail(ListCons(NumLit(5), ListCons(1, ListCons(2, ListNil))))) ;
  ];
  "ListIsNil", [
    test_e "1" (ListNil) ;
  ]
]
