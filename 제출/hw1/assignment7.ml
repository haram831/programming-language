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