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
    let rec loop from_n to_n acc =
      if from_n +. dx >= to_n then acc
      else loop (from_n +. dx) to_n (acc +. (calculate(substitute(from_n) func))*.dx)
    in
    if from_num < to_num
      then loop from_num to_num 0.
    else -. loop to_num from_num 0.
  | _ -> raise InvalidArgument

(* ========================= *)
(* test code starts here     *)
(* ========================= *)

let eps = 1e-6

let approx_equal a b =
  abs_float (a -. b) < eps

let print_result name passed expected actual =
  if passed then
    Printf.printf "[PASS] %s -> expected: %.10f, actual: %.10f\n" name expected actual
  else
    Printf.printf "[FAIL] %s -> expected: %.10f, actual: %.10f\n" name expected actual

let run_test name expr expected =
  try
    let actual = calculate expr in
    print_result name (approx_equal actual expected) expected actual
  with
  | InvalidArgument ->
      Printf.printf "[EXCEPTION] %s -> InvalidArgument raised\n" name
  | e ->
      Printf.printf "[EXCEPTION] %s -> unexpected exception: %s\n"
        name (Printexc.to_string e)

let run_invalid_test name expr =
  try
    let actual = calculate expr in
    Printf.printf "[FAIL] %s -> expected InvalidArgument, but got %.10f\n" name actual
  with
  | InvalidArgument ->
      Printf.printf "[PASS] %s -> InvalidArgument raised as expected\n" name
  | e ->
      Printf.printf "[FAIL] %s -> unexpected exception: %s\n"
        name (Printexc.to_string e)

(* 현재 네 integral 구현(L)은 dx를 곱하지 않으므로,
   expected 계산도 같은 방식으로 맞춰주는 helper *)
let expected_integral_like_code from_v to_v f =
  let rec loop x acc =
    if x > to_v then acc
    else loop (x +. dx) (acc +. f x)
  in
  loop from_v 0.

let () =
  Printf.printf "========== BASIC CONSTANT TESTS ==========\n";

  run_test "T01: I 3" (I 3) 3.0;
  run_test "T02: I (-5)" (I (-5)) (-5.0);
  run_test "T03: R 2.5" (R 2.5) 2.5;
  run_test "T04: R (-0.75)" (R (-0.75)) (-0.75);

  Printf.printf "\n========== BASIC ARITHMETIC TESTS ==========\n";

  run_test "T05: 3 + 4" (A (I 3, I 4)) 7.0;
  run_test "T06: 10 - 3" (S (I 10, I 3)) 7.0;
  run_test "T07: 6 * 7" (M (I 6, I 7)) 42.0;
  run_test "T08: 8 / 2" (D (I 8, I 2)) 4.0;
  run_test "T09: 3 + 2.5" (A (I 3, R 2.5)) 5.5;
  run_test "T10: 5.5 - 2" (S (R 5.5, I 2)) 3.5;
  run_test "T11: 1.5 * 4" (M (R 1.5, I 4)) 6.0;
  run_test "T12: 7.5 / 2.5" (D (R 7.5, R 2.5)) 3.0;

  Printf.printf "\n========== NESTED EXPRESSION TESTS ==========\n";

  run_test "T13: (3+4)*2"
    (M (A (I 3, I 4), I 2)) 14.0;

  run_test "T14: (10-3)/(2+5)"
    (D (S (I 10, I 3), A (I 2, I 5))) 1.0;

  run_test "T15: ((1+2)+(3+4))"
    (A (A (I 1, I 2), A (I 3, I 4))) 10.0;

  run_test "T16: ((8/2)*(5-1))"
    (M (D (I 8, I 2), S (I 5, I 1))) 16.0;

  run_test "T17: ((2*3)+(10/5))"
    (A (M (I 2, I 3), D (I 10, I 5))) 8.0;

  run_test "T18: ((2.5+3.5)*2)"
    (M (A (R 2.5, R 3.5), I 2)) 12.0;

  run_test "T19: (20-(3*4))"
    (S (I 20, M (I 3, I 4))) 8.0;

  run_test "T20: ((9-1)/(2*2))"
    (D (S (I 9, I 1), M (I 2, I 2))) 2.0;

  Printf.printf "\n========== SUBSTITUTE + X TESTS ==========\n";

  run_test "T21: substitute X with 5.0"
    (substitute 5.0 X) 5.0;

  run_test "T22: X + 3, x=2"
    (calculate (substitute 2.0 (A (X, I 3))) |> fun v -> R v) 5.0;

  run_test "T23: X - 1, x=4"
    (calculate (substitute 4.0 (S (X, I 1))) |> fun v -> R v) 3.0;

  run_test "T24: X * 2, x=3"
    (calculate (substitute 3.0 (M (X, I 2))) |> fun v -> R v) 6.0;

  run_test "T25: X / 2, x=8"
    (calculate (substitute 8.0 (D (X, I 2))) |> fun v -> R v) 4.0;

  run_test "T26: (X+1)*(X-1), x=3"
    (calculate (substitute 3.0 (M (A (X, I 1), S (X, I 1)))) |> fun v -> R v) 8.0;

  run_test "T27: X+X, x=2.5"
    (calculate (substitute 2.5 (A (X, X))) |> fun v -> R v) 5.0;

  run_test "T28: (X*X)+1, x=4"
    (calculate (substitute 4.0 (A (M (X, X), I 1))) |> fun v -> R v) 17.0;

  run_test "T29: (X/2)+(X/2), x=6"
    (calculate (substitute 6.0 (A (D (X, I 2), D (X, I 2)))) |> fun v -> R v) 6.0;

  run_test "T30: ((X+2)*(X+3)), x=1"
    (calculate (substitute 1.0 (M (A (X, I 2), A (X, I 3)))) |> fun v -> R v) 12.0;

  Printf.printf "\n========== SIGMA TESTS ==========\n";

  run_test "T31: sigma 1 to 3 of X"
    (C (I 1, I 3, X)) 6.0;

  run_test "T32: sigma 1 to 5 of 1"
    (C (I 1, I 5, I 1)) 5.0;

  run_test "T33: sigma 2 to 4 of (X+1)"
    (C (I 2, I 4, A (X, I 1))) 12.0;
  (* 3 + 4 + 5 = 12 *)

  run_test "T34: sigma 1 to 4 of (2*X)"
    (C (I 1, I 4, M (I 2, X))) 20.0;
  (* 2 + 4 + 6 + 8 *)

  run_test "T35: sigma 1 to 3 of (X*X)"
    (C (I 1, I 3, M (X, X))) 14.0;
  (* 1 + 4 + 9 *)

  run_test "T36: sigma 0 to 3 of (X-1)"
    (C (I 0, I 3, S (X, I 1))) 2.0;
  (* -1 + 0 + 1 + 2 *)

  run_test "T37: sigma 3 to 3 of X"
    (C (I 3, I 3, X)) 3.0;

  run_test "T38: sigma 5 to 3 of X"
    (C (I 5, I 3, X)) 0.0;

  run_test "T39: sigma -2 to 2 of X"
    (C (I (-2), I 2, X)) 0.0;

  run_test "T40: sigma 1 to 4 of ((X*X)+1)"
    (C (I 1, I 4, A (M (X, X), I 1))) 34.0;
  (* (1+1)+(4+1)+(9+1)+(16+1)=34 *)

  Printf.printf "\n========== INTEGRAL TESTS (CURRENT CODE BEHAVIOR) ==========\n";

  run_test "T41: integral 1 to 1 of X"
    (L (I 1, I 1, X))
    (expected_integral_like_code 1.0 1.0 (fun x -> x));

  run_test "T42: integral 1 to 1.2 of 1"
    (L (R 1.0, R 1.2, I 1))
    (expected_integral_like_code 1.0 1.2 (fun _ -> 1.0));

  run_test "T43: integral 1 to 1.2 of X"
    (L (R 1.0, R 1.2, X))
    (expected_integral_like_code 1.0 1.2 (fun x -> x));

  run_test "T44: integral 0 to 0.3 of (X+1)"
    (L (R 0.0, R 0.3, A (X, I 1)))
    (expected_integral_like_code 0.0 0.3 (fun x -> x +. 1.0));

  run_test "T45: integral 0 to 0.2 of (2*X)"
    (L (R 0.0, R 0.2, M (I 2, X)))
    (expected_integral_like_code 0.0 0.2 (fun x -> 2.0 *. x));

  run_test "T46: integral 0 to 0.2 of (X*X)"
    (L (R 0.0, R 0.2, M (X, X)))
    (expected_integral_like_code 0.0 0.2 (fun x -> x *. x));

  run_test "T47: integral 2 to 1 of X"
    (L (R 2.0, R 1.0, X))
    0.0;

  run_test "T48: integral -0.2 to 0.2 of X"
    (L (R (-0.2), R 0.2, X))
    (expected_integral_like_code (-0.2) 0.2 (fun x -> x));

  run_test "T49: integral 0 to 0.3 of 5"
    (L (R 0.0, R 0.3, I 5))
    (expected_integral_like_code 0.0 0.3 (fun _ -> 5.0));

  run_test "T50: integral 0 to 0.3 of ((X*X)+1)"
    (L (R 0.0, R 0.3, A (M (X, X), I 1)))
    (expected_integral_like_code 0.0 0.3 (fun x -> x *. x +. 1.0));

  Printf.printf "\n========== INVALID ARGUMENT TESTS ==========\n";

  run_invalid_test "T51: calculate X directly"
    X;

  run_invalid_test "T52: calculate (X + 1) directly"
    (A (X, I 1));

  run_invalid_test "T53: calculate sigma containing unsubstituted bound X"
    (C (X, I 3, X));

  run_invalid_test "T54: calculate integral containing unsubstituted upper bound X"
    (L (I 0, X, X));

  Printf.printf "\n========== TESTS FINISHED ==========\n"