type crazy3 =  NIL | ZERO of crazy3
  | ONE of crazy3 | MONE of crazy3
  | TWO of crazy3 | MTWO of crazy3

let rec crazy3add (a1, b1) = 
  match (a1, b1) with
  | (MTWO a2, MTWO b2) -> MONE(crazy3add ((MONE NIL), crazy3add (a2, b2)))
  | (MTWO a2, MONE b2) | (MONE a2, MTWO b2) -> ZERO(crazy3add ((MONE NIL), crazy3add (a2, b2)))
  | (MTWO a2, ZERO b2) | (ZERO a2, MTWO b2) -> MTWO(crazy3add(a2, b2))
  | (MTWO a2, ONE b2) | (ONE a2, MTWO b2)-> MONE(crazy3add(a2, b2))
  | (MTWO a2, TWO b2) | (TWO a2, MTWO b2) -> ZERO(crazy3add(a2, b2))
  | (MTWO a2, NIL) -> MTWO a2
  | (NIL, MTWO b2) -> MTWO b2

  | (MONE a2, MONE b2) -> MTWO(crazy3add (a2, b2))
  | (MONE a2, ZERO b2) | (ZERO a2, MONE b2) -> MONE(crazy3add (a2, b2))
  | (MONE a2, ONE b2) | (ONE a2, MONE b2)-> ZERO(crazy3add(a2, b2))
  | (MONE a2, TWO b2) | (TWO a2, MONE b2) -> ONE(crazy3add(a2, b2))
  | (MONE a2, NIL) -> MONE a2
  | (NIL, MONE b2) -> MONE b2

  | (ZERO a2, ZERO b2) -> ZERO(crazy3add (a2, b2))
  | (ZERO a2, ONE b2) | (ONE a2, ZERO b2) -> ONE(crazy3add (a2, b2))
  | (ZERO a2, TWO b2) | (TWO a2, ZERO b2) -> TWO(crazy3add(a2, b2))
  | (ZERO a2, NIL) -> ZERO a2
  | (NIL, ZERO b2) -> ZERO b2

  | (ONE a2, ONE b2) -> TWO(crazy3add(a2, b2))
  | (ONE a2, TWO b2) | (TWO a2, ONE b2) -> ZERO(crazy3add((ONE NIL), crazy3add(a2, b2)))
  | (ONE a2, NIL) -> ONE a2
  | (NIL, ONE b2) -> ONE b2
  
  | (TWO a2, TWO b2) -> ONE(crazy3add((ONE NIL), crazy3add(a2, b2)))
  | (TWO a2, NIL) -> TWO a2
  | (NIL, TWO b2) -> TWO b2

  | (NIL, NIL) -> NIL

(*test
let rec crazy3_to_string c =
  match c with
  | NIL -> "NIL"
  | ZERO x -> "ZERO(" ^ crazy3_to_string x ^ ")"
  | ONE x -> "ONE(" ^ crazy3_to_string x ^ ")"
  | MONE x -> "MONE(" ^ crazy3_to_string x ^ ")"
  | TWO x -> "TWO(" ^ crazy3_to_string x ^ ")"
  | MTWO x -> "MTWO(" ^ crazy3_to_string x ^ ")"

let rec crazy3val c =
  match c with
  | NIL -> 0
  | ZERO x -> 3 * crazy3val x
  | ONE x -> 1 + 3 * crazy3val x
  | MONE x -> -1 + 3 * crazy3val x
  | TWO x -> 2 + 3 * crazy3val x
  | MTWO x -> -2 + 3 * crazy3val x

let test name a b =
  let result = crazy3add (a, b) in
  Printf.printf "%s\n" name;
  Printf.printf "  a      = %s (=%d)\n" (crazy3_to_string a) (crazy3val a);
  Printf.printf "  b      = %s (=%d)\n" (crazy3_to_string b) (crazy3val b);
  Printf.printf "  result = %s (=%d)\n" (crazy3_to_string result) (crazy3val result);
  Printf.printf "  check  = %b\n\n" (crazy3val result = crazy3val a + crazy3val b)

let () =
  let n0 = NIL in
  let n1 = ONE NIL in
  let n2 = TWO NIL in
  let nm1 = MONE NIL in
  let nm2 = MTWO NIL in

  let x1 = ONE (ONE NIL) in
  let x2 = TWO (ONE NIL) in
  let x3 = MONE (ONE NIL) in
  let x4 = MONE (TWO (ZERO (ONE NIL))) in

  test "0 + 0" n0 n0;
  test "1 + 0" n1 n0;
  test "1 + 1" n1 n1;
  test "1 + 2" n1 n2;
  test "2 + 2" n2 n2;
  test "-1 + -1" nm1 nm1;
  test "-2 + -2" nm2 nm2;
  test "1 + -1" n1 nm1;
  test "2 + -2" n2 nm2;
  test "2 + -1" n2 nm1;
  test "11 + 1" x1 n1;
  test "5 + 4" x2 x1;
  test "2 + (-4)" n2 x3;
  test "custom sample" (ONE (TWO NIL)) x4
*)