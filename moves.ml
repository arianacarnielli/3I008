type move = {nom: char; move: int}

exception Cannot_move

let string_of_move m =
  let res = Bytes.create 2 in
  let () = Bytes.set res 0 m.nom in
  let () = Bytes.set res 1 (if m.move = -1 then '<' else '>') in
  Bytes.to_string res

let move_of_string str =
  {nom = str.[0]; move = (if str.[1] = '<' then -1 else 1)}

let apply_move m s =
  let boat = Port.get_boat m.nom s in
  let new_s = Port.remove_boat boat s in
  let boat = Port.move_boat boat m.move in
  try
    Port.add_boat boat new_s
  with
  |Invalid_argument _ -> raise Cannot_move

let win s =
  (Port.get_cell s 5 2) = Port.Rempli('A')

let list_move_of_string str =
  let rec loop nb acc =
    if nb >= String.length str then
      List.rev acc
    else
      loop (nb + 2) ((move_of_string (String.sub str nb 2))::acc)
  in loop 0 []

let check_solution s str =
  let list_move = list_move_of_string str in
  try
    let rec loop s_new l =
      Port.output_state s_new stdout;
      print_newline ();
      match l with
      |[] -> win s_new
      |hd::tl -> loop (apply_move hd s_new) tl
    in loop s list_move
  with
  |Cannot_move -> false
