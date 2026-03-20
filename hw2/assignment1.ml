(*자유전공학부 2023-13751 김하람*)
type exp = X
| I of int (* integer *)
| R of float (* real *)
| A of exp * exp (* add *)
| S of exp * exp (* subtract *)
| M of exp * exp (* multiply *)
| D of exp * exp (* divide *)
| C of exp * exp * exp (* sigma *)
| L of exp * exp * exp (* integral *)

exception InvalidArgument

let dx = 0.1

(*변수에 input 대입*)
let rec substitute x exp =
  match exp with
  | X -> R x
  | I n -> I n
  | R r -> R r
  | A (e1, e2) -> A (substitute x e1, substitute x e2)
  | S (e1, e2) -> S (substitute x e1, substitute x e2)
  | M (e1, e2) -> M (substitute x e1, substitute x e2)
  | D (e1, e2) -> D (substitute x e1, substitute x e2)
  | C (e1, e2, e3) -> C (substitute x e1, substitute x e2, substitute x e3)
  | L (e1, e2, e3) -> L (substitute x e1, substitute x e2, substitute x e3)

let rec calculate exp =
  match exp with
  | I x -> float_of_int x
  | R x -> x
  | A (exp1, exp2) -> calculate exp1 +. calculate exp2
  | S (exp1, exp2) -> calculate exp1 -. calculate exp2
  | M (exp1, exp2) -> calculate exp1 *.calculate exp2
  | D (exp1, exp2) -> calculate exp1 /.calculate exp2
  | C (from_exp, to_exp, func) 
    -> 
    let from_num = int_of_float (calculate from_exp) in
    let to_num = int_of_float(calculate to_exp) in
    let rec loop i acc =
      if i > to_num then acc
      else loop (i+1) (acc +. calculate(substitute(float_of_int i) func))
    in
    loop from_num 0.
  | L (from_exp, to_exp, func) 
    ->
    let from_num = calculate from_exp in
    let to_num = calculate to_exp in
    let rec loop i acc =
      if i > to_num then acc
      else loop (i +. dx) (acc +. calculate(substitute(i) func))
    in
    loop from_num 0.    
  | _ -> raise InvalidArgument

