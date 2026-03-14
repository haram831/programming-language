type crazy3 =  NIL | ZERO of crazy3
  | ONE of crazy3 | MONE of crazy3
  | TWO of crazy3 | MTWO of crazy3

let rec crazy3val n =
  match n with
  | NIL -> 0
  | ZERO a -> crazy3val a * 3
  | ONE a -> 1 + (crazy3val a)*3
  | MONE a -> -1 + (crazy3val a)*3
  | TWO a -> 2 + (crazy3val a)*3
  | MTWO a -> -2 + (crazy3val a)*3 

(*test
let a1 = ZERO NIL
let a2 = ZERO(ONE NIL)
let a3 = ONE(MTWO (TWO NIL))

let a4 = TWO(MONE(ZERO(MTWO(TWO NIL))))

let a5 = MONE(TWO(ONE(ZERO NIL)))

let () = 
  print_endline (string_of_int(crazy3val a1));
  print_endline (string_of_int(crazy3val a2));
  print_endline (string_of_int(crazy3val a3));
  print_endline (string_of_int(crazy3val a4));
  print_endline (string_of_int(crazy3val a5))
*)