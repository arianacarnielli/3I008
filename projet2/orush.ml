(******************************************************************************)
(* Fichier principal pour tester le solver                                    *)
(******************************************************************************)

if Array.length Sys.argv != 2 then
  print_endline ("Syntaxe: "^Sys.argv.(0)^" input_file")
else
  let file = open_in Sys.argv.(1) in
  let port = Port.input_state file in

  let () = print_endline "État initial :" in
  let () = Port.output_state port stdout in
  let () = print_newline () in

  let res = Solver.solve_input file in
  let () = print_endline "Solution :" in
  let () = print_endline res in
  let () = print_newline () in
  let () = print_endline "État final :" in

  let list_moves = Moves.list_move_of_string res in
  let final_state = List.fold_left (fun state move -> Moves.apply_move move state) port list_moves in
  let () = Port.output_state final_state stdout in
  let () = print_newline () in

  let () = print_endline "Vérification de la solution :" in
  let () = Printf.printf "%B\n" (Moves.check_solution port res) in
  close_in file
