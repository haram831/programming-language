let rec iter (n, f) = 
  match n with
  | n when n > 0 -> fun x -> f (iter ((n-1), f) x)
  | _ -> (fun x -> x)