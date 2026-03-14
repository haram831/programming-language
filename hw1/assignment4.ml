type team =  Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Norway | Sweden | England | Argentina
type tourna = LEAF of team | NODE of tourna * tourna

let team_to_string team = 
  match team with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize t = 
  match t with
  | LEAF team -> team_to_string team
  | NODE (a, b) -> "(" ^ parenize a ^ " " ^ parenize b ^ ")"

(* test
let t1 = LEAF Korea
let t2 = NODE (LEAF Korea, LEAF France)
let t3 =
  NODE (
    NODE (LEAF Korea, LEAF Usa),
    NODE (LEAF France, LEAF Brazil)
  )

let () =
  print_endline (parenize t1);
  print_endline (parenize t2);
  print_endline (parenize t3)
*)