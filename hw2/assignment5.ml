module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([],[])
    let enQ ((l, r), e) = (e::l, r)
    let deQ (l, r) =
      match r with
      | [] ->
        (match List.rev l with
         | [] -> raise EMPTY_Q
         | hd :: tl -> (hd, ([], tl)))
    | hd :: tl ->(hd, (l, tl))
  end