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
