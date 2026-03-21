(*자유전공학부 2023-13751 김하람*)

type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

exception InvalidArgument

let rec diff (ae, x) =
  match ae with
  | CONST c -> CONST 0
  | VAR s -> if s = x then CONST 1 else CONST 0
  | POWER (s, n) -> 
    if s <> x then CONST 0
    else 
    (match n with
    | 0 -> CONST 0
    | 1 -> CONST 1
    | _ -> TIMES [CONST (n); POWER (s, n-1)])
  | SUM exps ->
    (match exps with
    | [] -> raise InvalidArgument
    | _ -> SUM (List.map (fun e -> diff (e, x)) exps))
  | TIMES exps -> 
      (*곱의 미분법*)
      let rec aux left right =
        match right with
        | [] -> []
        | hd :: tl -> 
          let term = TIMES (List.rev_append left (diff (hd, x) :: tl)) in
          term :: aux (hd :: left) tl
      in
      (match exps with
      | [] -> raise InvalidArgument
      | _ -> SUM (aux [] exps))