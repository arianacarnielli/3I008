open Moves
open Port
open Solver

let a = boat_of_string "A2H12";;
let b = boat_of_string "B3V00";;
let c = boat_of_string "C2V23";;
let d = boat_of_string "D3H25";;
let e = boat_of_string "E3V30";;
let f = boat_of_string "F3H33";;
let g = boat_of_string "G2V54";;

let port = port_vide ();;
let port = add_boat a port;;
let port = add_boat b port;;
let port = add_boat c port;;
let port = add_boat d port;;
let port = add_boat e port;;
let port = add_boat f port;;
let port = add_boat g port;;

let moves = Solver.all_possible_moves port;;

let states = Solver.all_reachable_states port moves;;
let () = List.iter (fun s -> output_state s stdout; print_newline ()) states;;

let file = open_in "portFile2.txt";;
let port2 = input_state file;;
close_in file;;

let moves = Solver.all_possible_moves port2;;
let res = Solver.solve_state port2;;
let () = Port.output_state port2 stdout;;
