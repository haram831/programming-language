(*merge 함수*)
let rec merge (lst1, lst2) = 
  match lst1, lst2 with
  | [], [] -> []
  | head :: tail, [] -> lst1
  | [], head :: tail -> lst2
  | head1 :: tail1, head2::tail2 -> 
      if head1 > head2 
      then head1 :: merge (tail1, lst2)
      else head2 :: merge (lst1, tail2)