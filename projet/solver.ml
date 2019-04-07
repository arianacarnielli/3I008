(******************************************************************************)
(* solver.ml                                                                  *)
(******************************************************************************)

(******************************************************************************)
(* Fonction aditionnelle                                                      *)
(******************************************************************************)
(* val all_moves : Port.state -> Moves.move list *)
(* Renvoie la liste de tous les mouvements (possibles ou non) de tous les
   bateaux de l'état. *)
let all_moves s =
  let rec loop l acc =
    match l with
    |[] -> acc
    |hd::tl ->
      let nom = (Char.escaped (Port.string_of_boat hd).[0]) in
      loop tl ((Moves.move_of_string (nom ^ ">")) :: ((Moves.move_of_string (nom ^ "<")) :: acc))
  in loop s []

(******************************************************************************)
(* Fonctions demandées                                                        *)
(******************************************************************************)
(* val all_possible_moves : Port.state -> Moves.move list *)
(* Calcul de tous les mouvements possibles depuis un état. *)
let all_possible_moves s =
  let moves = all_moves s in
  let filtre m =
    try
      let _ = Moves.apply_move m s in
      true
    with
    | Moves.Cannot_move -> false
  in List.filter filtre moves

(* val all_reachable_states : Port.state -> Moves.move list
   -> Port.state list *)
(* Calcul de tous les états atteignables depuis un état et une liste de move. *)
let all_reachable_states s list_m =
  List.map (fun m -> Moves.apply_move m s) list_m

(* val solve_state : Port.state -> string *)
(* Calcul d’une solution à partir d’un état. *)
let solve_state s =
  (* Les états déjà visités sont stockés dans une table de hachage pour ne pas
     les revisiter. Ils sont stockés sous forme de chaîne de caractères à
     l'aide de la fonction Port.string_of_state. *)
  let visite = Hashtbl.create 100 in
  (* La queue contient des paires avec les états de la bordure et la suite de
     mouvements pour y arriver (sous forme de chaîne de caractères). *)
  let file = Queue.create () in
  let () = Queue.add (s, "") file in
  let () = Hashtbl.add visite (Port.string_of_state s) () in

  let rec start () =
    if Queue.is_empty file then
      raise Not_found
    else
      let s_cour = Queue.take file in
      match s_cour with
      |(state, str) ->
        if Moves.win state then
          str
        else
          let l_move = all_possible_moves state in
          let l_state = all_reachable_states state l_move in
          let ajoute m ss =
            if not (Hashtbl.mem visite (Port.string_of_state ss)) then
              let () = Hashtbl.add visite (Port.string_of_state ss) () in
              Queue.add (ss, str ^ (Moves.string_of_move m)) file in
          let () = List.iter2 (ajoute) l_move l_state in
          start ()
  in start ()

(* val solve_input : in_channel -> string *)
(* Calcul d’une solution à partir d’un canal d’entrée qui contient une
   représentation de l’état initial. *)
let solve_input in_ch =
  seek_in in_ch 0;
  solve_state (Port.input_state in_ch)
