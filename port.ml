type cell = Vide| Rempli of char

type state = cell array array

type boat = {nom: char; pos: char; taille: int; x: int; y: int}

let boat_of_string str =
  {nom = str.[0]; pos = str.[2]; taille = (int_of_char str.[1] - 48); x = (int_of_char str.[3] - 48); y = (int_of_char str.[4] - 48)}

let string_of_boat  boat =
  let str = ref "" in
  str := !str ^ Char.escaped boat.nom;
  str := !str ^ Char.escaped (char_of_int (boat.taille + 48));
  str := !str ^ Char.escaped boat.pos;
  str := !str ^ Char.escaped (char_of_int (boat.x + 48));
  str := !str ^ Char.escaped (char_of_int (boat.y + 48));
  !str

let est_rempli c =
  match c with
  |Vide -> false
  |Rempli _ -> true

let add_boat c s =
  for i = c.x to (c.x + c.taille - 1) do
    if (est_rempli s.(i).(c.y)) then
      raise (Invalid_argument "add_boat")
    else
      print_endline("vide")
  done
;;





let c = boat_of_string "A2H01";;
let b = boat_of_string "B2V20";;

let s = [|
  [|Vide; Vide; Vide|];
  [|Rempli('A'); Rempli('A'); Vide|];
  [|Vide; Vide; Vide|]
|];;
