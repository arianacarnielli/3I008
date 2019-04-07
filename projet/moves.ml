(******************************************************************************)
(* moves.ml                                                                   *)
(******************************************************************************)

type move = {nom: char; move: int}

exception Cannot_move

(******************************************************************************)
(* Fonctions demandées                                                        *)
(******************************************************************************)
(* val string_of_move : move -> string *)
(* Convertit un move en chaine de caracteres. *)
let string_of_move m =
  let res = Bytes.create 2 in
  let () = Bytes.set res 0 m.nom in
  let () = Bytes.set res 1 (if m.move = -1 then '<' else '>') in
  Bytes.to_string res

(* val move_of_string : string -> move *)
(* Convertit une chaine de caracteres en move. *)
let move_of_string str =
  {nom = str.[0]; move = (if str.[1] = '<' then -1 else 1)}

(* val apply_move : move -> Port.state -> Port.state *)
(* apply_move m s applique le mouvement m depuis l’état s et retourne le nouvel
   état. Lève l’exception Cannot_move si le mouvement est impossible. *)
let apply_move m s =
  let boat = Port.get_boat m.nom s in
  let new_s = Port.remove_boat boat s in
  let boat = Port.move_boat boat m.move in
  try
    Port.add_boat boat new_s
  with
  |Invalid_argument _ -> raise Cannot_move

(* val win : Port.state -> bool *)
(* Indique si un état est une position gagnante. *)
let win s =
  let boat = Port.get_boat 'A' s in
  (Port.x_y_of_boat boat) = (4, 2)

(******************************************************************************)
(* Fonction aditionnelle                                                      *)
(******************************************************************************)
(* val list_move_of_string : string -> move list *)
(* Crée une liste de mouvements à partir d'une string représentant des
   mouvements. *)
let list_move_of_string str =
  let rec loop nb acc =
    if nb >= String.length str then
      List.rev acc
    else
      loop (nb + 2) ((move_of_string (String.sub str nb 2))::acc)
  in loop 0 []

(******************************************************************************)
(* Fonction demandée                                                          *)
(******************************************************************************)

(* val check_solution : Port.state -> string -> bool *)
(* Vérifie si une suite de mouvements depuis l’état en paramètre est une
   solution. *)
let check_solution s str =
  let list_move = list_move_of_string str in
  try
    let rec loop s_new l =
      match l with
      |[] -> win s_new
      |hd::tl -> loop (apply_move hd s_new) tl
    in loop s list_move
  with
  |Cannot_move -> false
