open Moves
open Port
open Solver

let file = open_in "tests/pos1.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos2.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos3.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos4.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos5.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos6.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos7.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos8.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos9.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos10.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos11.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file

let file = open_in "tests/pos12.txt"
let port = Port.input_state file
let () = Port.output_state port stdout
let res = Solver.solve_input file
let () = print_endline res
let () = Printf.printf "%B" (check_solution port res)
let () = print_newline ()
let () = close_in file
