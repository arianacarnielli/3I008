let f = 1;;
let xor = fun x y -> (x || y) && (not(x && y));;

let x = true;;
let y = false;;

let xor2 x y =
  if x then
    (not y)
  else
    y
let xor3 x y =
  match x, y with
    true, true -> false
  | true, false -> true
  | false, true -> true
  | false, false -> false

let xor4 x y =
  match x, y with
  | true, false -> true
  | false, true -> true
  | _ -> false

let xor5 x y =
  match x, y with
  | true, true -> false
  | true, false -> true
  | false, r -> r

let rec puissance x y =
  if y < 0 then
    failwith "invalid exponent"
  else if y = 0 then
    1
  else
    x * (puissance x (y - 1))

let pow x y =
  let rec loop y =
    if y = 0 then
      1
    else x * (loop (y - 1))
  in
  if y < 0 then
    raise (Invalid_argument "negative exponent")
  else
    loop y

let pow x y =
  let rec loop y acc =
    if y = 0 then
      acc
    else (loop (y - 1) (x * acc))
  in
  if y < 0 then
    raise (Invalid_argument "negative exponent")
  else
    loop y 1

let shift_char c d =
  char_of_int (((int_of_char c) + d) mod 255)

let shift c =
  shift_char c 1;;

let code_cesar str d =
  String.map (fun c -> (shift_char c d)) str

let encode_cesari str =
  String.mapi (fun i c -> (shift_char c i)) str

let decode_cesari str =
  String.mapi (fun i c -> (shift_char c (-i))) str

let string_foldi str a f =
  let len = String.length str in
  let rec loop i r =
    if(i < len) then
      (loop (i+1) (f i str.[i] r))
    else
      r
  in(loop 0 a)

let checksum2 str =
  (string_foldi str 0 (fun i c r -> (int_of_char c)*i + r))


let chaine = "abcde"
