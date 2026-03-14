type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec expr_decode e =
  match e with
  | NUM a -> a
  | PLUS (a, b) -> expr_decode a + expr_decode b
  | MINUS (a, b) -> expr_decode a - expr_decode b

let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT a -> not (eval a)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) -> if (eval a) then if (eval b) then true else false else true
  | LESS (a, b) -> expr_decode a < expr_decode b

(*test
let f1 = TRUE
let f2 = NOT FALSE
let f3 = ANDALSO (TRUE, FALSE)
let f4 = ORELSE (FALSE, TRUE)
let f5 = IMPLY (TRUE, FALSE)
let f6 = IMPLY (FALSE, FALSE)

let f7 =
  LESS (
    PLUS (NUM 3, NUM 4),
    MINUS (NUM 10, NUM 1)
  )

let f8 =
  ANDALSO (
    LESS (NUM 3, NUM 5),
    ORELSE (FALSE, TRUE)
  )

let () =
  print_endline (string_of_bool (eval f1));
  print_endline (string_of_bool (eval f2));
  print_endline (string_of_bool (eval f3));
  print_endline (string_of_bool (eval f4));
  print_endline (string_of_bool (eval f5));
  print_endline (string_of_bool (eval f6));
  print_endline (string_of_bool (eval f7));
  print_endline (string_of_bool (eval f8))

*)