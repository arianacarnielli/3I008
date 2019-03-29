type cell = Vide| Rempli of char

type state = cell array array

type boat = {nom: char; pos: char; taille: int; mutable x: int; mutable y: int}

let boat_of_string str =
  {nom = str.[0]; pos = str.[2]; taille = (int_of_char str.[1] - 48); x = (int_of_char str.[3] - 48); y = (int_of_char str.[4] - 48)}

let string_of_boat  boat =
  let res = Bytes.create 5 in
  let () = Bytes.set res 0 boat.nom in
  let () = Bytes.set res 1 (char_of_int (boat.taille + 48)) in
  let () = Bytes.set res 2 boat.pos in
  let () = Bytes.set res 3 (char_of_int (boat.x + 48)) in
  let () = Bytes.set res 4 (char_of_int (boat.y + 48)) in
  Bytes.to_string res

let est_rempli cellule =
  match cellule with
  |Vide -> false
  |Rempli _ -> true

let add_boat b s =
  let new_s = Array.map (Array.copy) s in
    let rec loop x y dx dy taille =
      match taille with
      | 0 -> new_s
      | _ -> if (est_rempli s.(y).(x)) then
              raise (Invalid_argument "add_boat")
            else
              new_s.(y).(x) <- Rempli(b.nom);
              loop (x + dx) (y + dy) dx dy (taille - 1)
    in
      match b.pos with
      |'H' -> loop b.x b.y 1 0 b.taille
      |_ -> loop b.x b.y 0 1 b.taille

let char_of_cell cellule =
  match cellule with
  |Vide -> '~'
  |Rempli(lettre) -> lettre

let grid_of_state state =
  Array.map (Array.map char_of_cell) state

let port_vide () =
  Array.init 6 (fun _ -> Array.make 6 Vide)

let input_state in_c =
  let res = ref (port_vide ()) in
    try
      while true do
        let line = input_line in_c in
        let boat = boat_of_string line in
        res:= add_boat boat !res
      done;
      !res
    with
    | End_of_file -> !res


let output_state state out =
  Array.iter (fun ligne -> Array.iter (fun cell -> output_char out (char_of_cell cell))ligne; output_char out '\n') state

let c = boat_of_string "C2H01";;
let b = boat_of_string "B2V20";;

let s = [|
  [|Vide; Vide; Vide|];
  [|Rempli('A'); Rempli('A'); Vide|];
  [|Vide; Vide; Vide|]
|];;
