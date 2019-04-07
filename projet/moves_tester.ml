open Moves
open Port

let a = boat_of_string "A2H12"
let b = boat_of_string "B3V00"
let c = boat_of_string "C2V23"
let d = boat_of_string "D3H25"
let e = boat_of_string "E3V30"
let f = boat_of_string "F3H33"
let g = boat_of_string "G2V54"

let port = []
let port = add_boat a port
let port = add_boat b port
let port = add_boat c port
let port = add_boat d port
let port = add_boat e port
let port = add_boat f port
let port = add_boat g port

let move_test = {nom = 'A'; move = 1}

let () = print_endline (string_of_move move_test)

let res = "D<B>B>B>A<C<C<F<C<F<A>B<D<B<B<F<E>E>E>A>A>A>"

let () = Printf.printf "%B\n" (check_solution port res)
