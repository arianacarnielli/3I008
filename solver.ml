let all_moves s =
  let list_bat = Port.get_bateaux s in
  let rec loop l acc =
    match l with
    |[] -> acc
    |hd::tl ->
      let nom = (Char.escaped (Port.string_of_boat hd).[0]) in
      loop tl ((Moves.move_of_string (nom ^ ">")) :: ((Moves.move_of_string (nom ^ "<")) :: acc))
  in loop list_bat []

let all_possible_moves s =
  let moves = all_moves s in
  let filtre m =
    try
      let _ = Moves.apply_move m s in
      true
    with
    | Moves.Cannot_move -> false
  in List.filter filtre moves

let all_reachable_states s list_m =
  List.map (fun m -> Moves.apply_move m s) list_m

(* calcul d’une solution a partir d’un etat *)
let solve_state s =
  let visite = Hashtbl.create 100 in
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

let solve_input in_ch =
  solve_state (Port.input_state in_ch)



    (**)
