open Port

let s = [|
  [|Vide; Vide; Vide|];
  [|Rempli('A'); Rempli('A'); Vide|];
  [|Vide; Vide; Vide|]
|];;


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

output_state port stdout;;

let d = get_boat 'D' port;;
let port = remove_boat d port;;
let d = move_boat d (-1);;
let port = add_boat d port;;

output_state port stdout;;

let b = get_boat 'B' port;;
let port = remove_boat b port;;
let b = move_boat b (1);;
let port = add_boat b port;;

output_state port stdout;;

let file = open_in "portFile1.txt";;
let port = input_state file;;
close_in file;;

output_state port stdout;;

let str = Port.string_of_state port;;
