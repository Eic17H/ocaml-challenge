type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let alf2023 = [
  { id="60/61/65570"; name="Ambra"; surname="Ambu"; vote=Some 21; laude=false };
  { id="61/61/65778"; name="Brunello"; surname="Brundu"; vote=Some 18; laude=false };
  { id="60/61/65624"; name="Costantino"; surname="Cossu"; vote=Some 24; laude=false };
  { id="60/61/65808"; name="Deborah"; surname="Demurtas"; vote=Some 28; laude=false };
  { id="60/61/65668"; name="Efisio"; surname="Ennas"; vote=Some 18; laude=false };
  { id="60/61/65564"; name="Felicino"; surname="Frau"; vote=None; laude=false };
  { id="60/64/20203"; name="Gavino"; surname="Girau"; vote=Some 20; laude=false };
  { id="60/61/65892"; name="heidi"; surname="hernandez"; vote=Some 8; laude=true };
  { id="60/61/65563"; name="Igino igor"; surname="Ibba"; vote=Some 15; laude=false };
  { id="60/61/64427"; name="Lillo"; surname="Lilliu"; vote=Some 25; laude=false };
  { id="60/61/65448"; name="Morgan"; surname="Murtas"; vote=Some 15; laude=false };
  { id="61/61/65213"; name="Nathan"; surname="Nieddu"; vote=Some 16; laude=false };
  { id="60/61/65832"; name="Ornella"; surname="Onnis"; vote=Some 30; laude=true };
  { id="60/61/65517"; name="Pinuccio"; surname="Puddu"; vote=Some 28; laude=false };
  { id="60/64/21222"; name="Quintilio"; surname="Quaglioni"; vote=Some 22; laude=false };
  { id="60/61/65907"; name="Rihanna"; surname="Ruzzu"; vote=Some 18; laude=false };
  { id="60/61/65766"; name="Samantah"; surname="Sulis"; vote=Some 30; laude=false };
  { id="60/61/65730"; name="Tatiana"; surname="Truzzu"; vote=Some 30; laude=true };
  { id="60/61/65738"; name="Ubaldo"; surname="Urru"; vote=None; laude=true };
  { id="60/61/65722"; name="Valentina"; surname="Vargiu"; vote=Some 30; laude=true };
  { id="60/61/65592"; name="Zlatan"; surname="Zuncheddu"; vote=Some 18; laude = false }
];;

let id_of_noshow students =
  students
  |> List.filter (fun a -> a.vote = None)
  |> List.map (fun a -> a.id)
;;

let upgradeable students =
  students
  |> List.filter (fun a -> a.vote >= Some 15 && a.vote <= Some 17)
  |> List.map (fun a -> a.id)
;;

let upgrade students =
  students
  |> List.map (fun a ->
    if a.vote >= Some 15 && a.vote <= Some 17
      then {id = a.id; name = a.name; surname = a.surname; vote = Some 18; laude = a.laude}
      else a
  )
;;

let wrong_laude students =
  students
  |> List.filter (fun a -> a.laude = true && not (a.vote == Some 30))
  |> List.map (fun a -> String.concat a.name [" "; a.surname])
;;

let fix_laude students =
  students
  |> List.map (fun a ->
    if a.laude = true && not (a.vote == Some 30)
      then {id = a.id; name = a.name; surname = a.surname; vote = a.vote; laude = false}
  else a
  )
;;

let percent_passed students =
  (
    students
    |> List.filter (fun a -> a.vote >= Some 18)
    |> List.map (fun a -> 1)
    |> List.fold_left ( + ) 0
  )
  *
  100
  /
  (
    students
    |> List.map (fun a -> 1)
    |> List.fold_left ( + ) 0
  )
;;

let avg_vote students =
  (float_of_int (students
  |> List.filter (fun a -> a.vote >= Some 18)
  |> List.map (fun a -> match a.vote with None -> 0 | Some b -> b + if a.laude then 2 else 0)
  |> List.fold_left ( + ) 0))
  /.
  (float_of_int (students
  |> List.map (fun a -> 1)
  |> List.fold_left ( + ) 0))
;;