type cell = Vide| Rempli of char

type boat = {nom: char; pos: char; taille: int; x: int; y: int}

type state = {monde: cell array array; bateaux: boat list}

let get_bateaux s =
  s.bateaux

let get_cell s x y =
  s.monde.(y).(x)

let est_rempli s x y =
  if x >= 0 && y >=0 && x < Array.length s.monde.(0) && y < Array.length s.monde then
    match s.monde.(y).(x) with
      |Vide -> false
      |Rempli _ -> true
  else
    true

let char_of_cell cellule =
  match cellule with
  |Vide -> '~'
  |Rempli(lettre) -> lettre

let string_of_state s =
  let buff = Buffer.create 36 in
  Array.iter (fun ligne -> Array.iter (fun cellule -> Buffer.add_char buff (char_of_cell cellule)) ligne) s.monde;
  Buffer.contents buff

let port_vide () =
  {monde = Array.init 6 (fun _ -> Array.make 6 Vide); bateaux = []}

let remove_boat b s =
  {monde = Array.map (
    fun ligne -> Array.map (
        fun cellule -> match cellule with
          |Rempli(nom) ->
            if nom = b.nom then
              Vide
            else
              Rempli(nom)
          |Vide -> Vide
      ) ligne
  ) s.monde; bateaux = List.filter (fun boat -> boat.nom <> b.nom) s.bateaux}

let get_boat c s =
  List.find (fun b -> b.nom = c) s.bateaux

let move_boat b nb =
  if b.pos = 'H' then
    {nom = b.nom; pos = b.pos; taille = b.taille; x = b.x + nb; y = b.y}
  else
    {nom = b.nom; pos = b.pos; taille = b.taille; x = b.x; y = b.y + nb}

let boat_of_string str =
  {nom = str.[0]; pos = str.[2]; taille = (int_of_char str.[1] - 48); x = (int_of_char str.[3] - 48); y = (int_of_char str.[4] - 48)}

let string_of_boat boat =
  let res = Bytes.create 5 in
  let () = Bytes.set res 0 boat.nom in
  let () = Bytes.set res 1 (char_of_int (boat.taille + 48)) in
  let () = Bytes.set res 2 boat.pos in
  let () = Bytes.set res 3 (char_of_int (boat.x + 48)) in
  let () = Bytes.set res 4 (char_of_int (boat.y + 48)) in
  Bytes.to_string res

let add_boat b s =
  let new_monde = Array.map (Array.copy) s.monde in
    let rec loop x y dx dy taille =
      match taille with
      | 0 -> {monde = new_monde; bateaux = b::(s.bateaux)}
      | _ -> if (est_rempli s x y) then
              raise (Invalid_argument "add_boat")
            else
              new_monde.(y).(x) <- Rempli(b.nom);
              loop (x + dx) (y + dy) dx dy (taille - 1)
    in
      match b.pos with
      |'H' -> loop b.x b.y 1 0 b.taille
      |_ -> loop b.x b.y 0 1 b.taille

let grid_of_state s =
  Array.map (Array.map char_of_cell) s.monde

let input_state in_ch =
  let res = ref (port_vide ()) in
    try
      while true do
        let line = input_line in_ch in
        let boat = boat_of_string line in
        res:= add_boat boat !res
      done;
      !res
    with
    | End_of_file -> !res

let output_state s out =
  Array.iter (fun ligne -> Array.iter (fun cell -> output_char out (char_of_cell cell))ligne; output_char out '\n') s.monde
