let rec print_int_list l =
  if l = [] then ()
  else
    ((print_int (List.hd l)) ; print_int_list (List.tl l))



let l = [1;2;3;4;5;6;7;8];;
let _ = print_int_list l;;
let _ = print_endline " ";;

let rec interval n m =
  if n = m then [n]
  else begin
    if n>m then []
    else
      n::(interval (n + 1) m)
  end;;

let _ = print_int_list (interval 2 4)
let _ = print_endline " ";;

let rec filter_out p l =
  if l = [] then []
  else begin
    if not (p (List.hd l)) then (List.hd l)::filter_out p (List.tl l)
    else
      filter_out p (List.tl l)
  end;;

let is_multiple m x =
  if (m mod x) = 0 then true
  else
    false

let x = if(is_multiple 1 2) then 1 else 2;;
print_int(x);;
let _ = print_endline " ";;

let rec remove_multiple_of n l =
  filter_out(fun m -> is_multiple m n) l

let _ = print_int_list (remove_multiple_of 2 l);;
let _ = print_endline " ";;
