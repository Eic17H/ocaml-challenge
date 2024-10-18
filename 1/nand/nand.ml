let nand_connective a b = not(a&&b);;
let nand_conditional a b = if a then (if b then false else true) else true;;
let nand_pattern a b = match [a,b] with
  | [true,true] -> false
  | _ -> true;;