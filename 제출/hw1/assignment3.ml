(*
일의 자리 -> 1
십의 자리 -> 2
백의 자리 -> 3
천의 자리 -> 4
*)

let decode n i =
  let word = [|"일"; "이"; "삼"; "사"; "오"; "육"; "칠"; "팔"; "구"|] in
  let digits = [|"십"; "백"; "천"|] in
  if n = 0 then []
  else if i = 1 then [word.(n - 1)]
  else if n = 1 then [digits.(i - 2)]
  else [word.(n - 1); digits.(i - 2)]

let translate str =
  let len = String.length str in
  let rec make i =
    if i = len then []
    else
      let n = Char.code str.[i] - Char.code '0' in
      let pos = len - i in
      decode n pos @ make (i + 1)
  in
  if len = 3 || len = 4 then
    let r = make 0 in
    if r = [] then ["영"] else r
  else []

let vocalize str =
  match str with
  | str when String.length str = 8 ->
      [translate (String.sub str 0 4); translate (String.sub str 4 4)]
  | str when String.length str = 7 ->
      [translate (String.sub str 0 3); translate (String.sub str 3 4)]
  | _ -> []