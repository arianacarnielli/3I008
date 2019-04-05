open Moves
open Port
open Solver


let file = open_in "tests/pos1.txt";;
let port = Port.input_state file;;

(*let () = Port.output_state port stdout*)
(*let res = Solver.solve_state port;;*)

let res = Solver.solve_input file;;
print_endline res;;
let () = Printf.printf "%B" (check_solution port res);;
print_newline ();;
close_in file;;
