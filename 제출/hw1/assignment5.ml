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
