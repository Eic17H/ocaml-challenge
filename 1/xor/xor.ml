let xor_connective a b = (a||b)&&not(a&&b);;
let xor_conditional a b = if a then (if b then false else true) else (if not b then false else true);;
let xor_pattern a b = match [a,b] with
  | [true,true] -> false
  | [false,false] -> false
  | _ -> true;;