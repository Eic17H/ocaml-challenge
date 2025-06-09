let fib n =
  let rec aux n = 
    match n with
    | 1 -> [0]
    | 2 -> [1; 0]
    | _ ->
      let prec = fib (n-1) in
      (List.nth prec 0 + List.nth prec 1) :: prec
    in List.rev (aux n)
;;