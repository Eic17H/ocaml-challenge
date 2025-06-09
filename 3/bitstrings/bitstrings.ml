type bitstring = E | Z of bitstring | U of bitstring;;

let string_of_bitstring bs =
  let rec aux bs str =
    match bs with
    | E -> ""
    | U a -> String.concat "" ["1"; aux a str]
    | Z a -> String.concat "" ["0"; aux a str]
  in aux bs ""
;;

let len bs =
  let rec aux bs lng =
    match bs with
    | E -> 0
    | U a -> 1 + aux a lng
    | Z a -> 1 + aux a lng
  in aux bs 0
;;

let countZ bs =
  let rec aux bs lng =
    match bs with
    | E -> 0
    | U a -> 0 + aux a lng
    | Z a -> 1 + aux a lng
  in aux bs 0
;;

let countU bs =
  let rec aux bs lng =
    match bs with
    | E -> 0
    | U a -> 1 + aux a lng
    | Z a -> 0 + aux a lng
  in aux bs 0
;;

let concat bsx bsy =
  let rec aux bsx bsy =
    match bsx with
    | E -> bsy
    | U a -> U (aux a bsy)
    | Z a -> Z (aux a bsy)
  in aux bsx bsy
;;

let equals bsx bsy =
  let rec aux bsx bsy =
    match (bsx, bsy) with
    | (E, E) -> true
    | (U a, U b) -> aux a b
    | (Z a, Z b) -> aux a b
    | _ -> false
  in aux bsx bsy
;;

let tl bs = 
  let rec aux bs =
    match bs with
    | E -> E
    | U a -> a
    | Z a -> a
  in aux bs
;;