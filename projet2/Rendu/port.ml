(******************************************************************************)
(* port.ml                                                                    *)
(******************************************************************************)

type boat = {nom: char; pos: char; taille: int; x: int; y: int}

type state = boat list

(******************************************************************************)
(* Fonctions aditionnelles                                                    *)
(******************************************************************************)

(* val list_case_of_boat : boat -> (int * int) list *)
(* Renvoie la liste des coordonnées (x, y) occupées par le bateau b. *)
let list_case_of_boat b =
  let rec loop x y dx dy taille acc =
    match taille with
    | 0 -> acc
    | _ -> loop (x + dx) (y + dy) dx dy (taille - 1) ((x, y)::acc)
  in
  match b.pos with
  |'H' -> loop b.x b.y 1 0 b.taille []
  |_ -> loop b.x b.y 0 1 b.taille []

(* val get_boat : char -> state -> boat *)
(* Renvoie le bateau dont le nom est c dans l'état s. *)
let get_boat c s =
  List.find (fun b -> b.nom = c) s

(* val remove_boat : boat -> state -> state *)
(* Supprime le bateau b de l'état s. *)
let remove_boat b s =
  List.filter (fun boat -> boat.nom <> b.nom) s

(* val move_boat : boat -> int -> boat *)
(* Renvoie un nouveau bateau correspondant au bateau b bougé de nb. La valeur
   de nb doit être +1 ou -1 et le mouvement se fait suivant l'orientation du
   bateau ('H' ou 'V').*)
let move_boat b nb =
  if b.pos = 'H' then
    {nom = b.nom; pos = b.pos; taille = b.taille; x = b.x + nb; y = b.y}
  else
    {nom = b.nom; pos = b.pos; taille = b.taille; x = b.x; y = b.y + nb}

(* val x_y_of_boat : boat -> int * int *)
(* Renvoie les coordonnées (x, y) de la première case du bateau b. *)
let x_y_of_boat b =
  (b.x, b.y)

let pos_of_boat b =
  b.pos

let taille_of_boat b =
  b.taille

(* val boat_in_grid : boat -> bool *)
(* Renvoie true si et seulement si le bateau b ne sort pas de la grille. *)
let boat_in_grid b =
  List.for_all (
    fun pos ->
      match pos with
      | (x, y) -> x >= 0 && y >= 0 && x < 6 && y < 6
  ) (list_case_of_boat b)

(******************************************************************************)
(* Fonctions demandées                                                        *)
(******************************************************************************)

(* val boat_of_string : string -> boat *)
(* Transforme une chaine (ex : ‘‘A2H45’’) en une valeur de type boat. *)
let boat_of_string str =
  {nom = str.[0]; pos = str.[2]; taille = (int_of_char str.[1] - 48); x = (int_of_char str.[3] - 48); y = (int_of_char str.[4] - 48)}

(* val string_of_boat : boat -> string *)
(* Représente un bateau sous la forme definie dans le sujet (ex : ‘‘A2H45’’). *)
let string_of_boat boat =
  let res = Bytes.create 5 in
  let () = Bytes.set res 0 boat.nom in
  let () = Bytes.set res 1 (char_of_int (boat.taille + 48)) in
  let () = Bytes.set res 2 boat.pos in
  let () = Bytes.set res 3 (char_of_int (boat.x + 48)) in
  let () = Bytes.set res 4 (char_of_int (boat.y + 48)) in
  Bytes.to_string res

(* val add_boat : boat -> state -> state *)
(* add_boat c s ajoute à l’état s un nouveau bateau c. Leve Invalid_argument
   ‘‘add_boat’’ si le bateau ne peut être mis à cette position. *)
let add_boat b s =
  if not (boat_in_grid b) then
    raise (Invalid_argument "add_boat")
  else
    let pos_b = list_case_of_boat b in
    let () = List.iter (fun boat ->
        let pos_boat = list_case_of_boat boat in
        List.iter (fun cell ->
            if List.mem cell pos_boat then
              raise (Invalid_argument "add_boat")
          ) pos_b
      ) s
    in b::s

(* val grid_of_state : state -> char array array *)
(* Retourne une représentation matricielle d’un état du port. *)
let grid_of_state s =
  let monde = Array.init 6 (fun _ -> Array.make 6 '~') in
  let () = List.iter (fun boat ->
    List.iter (fun case ->
      match case with
	  | (x, y) -> monde.(y).(x) <- boat.nom;
    ) (list_case_of_boat boat)
  ) s in
  monde

(******************************************************************************)
(* Fonction aditionnelle                                                      *)
(******************************************************************************)
(* val string_of_state : state -> string *)
(* Crée une string depuis un state s basée sur la grille que le state s
   représente. Utilisée comme hash. *)
let string_of_state s =
  let buff = Buffer.create 36 in
  Array.iter (fun ligne -> Array.iter (fun cell -> Buffer.add_char buff cell) ligne) (grid_of_state s);
  Buffer.contents buff

(******************************************************************************)
(* Fonctions demandées                                                        *)
(******************************************************************************)
(* val input_state : in_channel -> state *)
(* Transforme une représentation de l’état initial du port depuis un canal
   d’entrée vers une valeur de type state. *)
let input_state in_ch =
  seek_in in_ch 0;
  let rec loop acc =
    try
      let line = input_line in_ch in
      let boat = boat_of_string line in
      loop (boat::acc)
    with
    | End_of_file -> acc
  in loop []

(* val output_state : state -> out_channel -> unit *)
(* Imprime une représentation matricielle d’un état du port sur un canal de
   sortie. *)
let output_state s out =
  let monde = grid_of_state s in
  Array.iter (fun ligne -> Array.iter (fun cell -> output_char out cell)ligne; output_char out '\n') monde
