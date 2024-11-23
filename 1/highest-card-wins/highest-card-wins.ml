(* non so come passare un valore intero alla funzione: This expression has type int but an expression was expected of type card *)
type card = Joker | Val of int;;
let jokerLose a b = (b=Joker);; (* if the dealer has the Joker, the dealer wins *)
let jokerWin a b = (a=Joker) && (not (b=Joker));; (* if only the player has the Joker, the player wins *)
let valueWin a b = (a>b);;
let win a b = (not (jokerLose a b)) && ((jokerWin a b) || (valueWin a b));;