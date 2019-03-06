let rec print_int_list l =
  match l with
  |[] -> print_char ('\n')
  | hd::tl -> print_int (hd); print_char(' '); print_int_list (tl);;

let test_list = [1;2;3;4;5];;

(*print_int_list test_list;;*)

let interval n m =
  let rec loop n m acc =
    if n > m then
      List.rev acc
    else
      loop (n+1) m (n::acc)
  in loop n m [];;

let filter_out p l =
  let rec loop p l acc =
    match l with
    |[] -> List.rev acc
    |hd::tl -> if p hd then
        loop p tl acc
      else
        loop p tl (hd::acc)
  in loop p l [];;

let is_multiple m x =
  m mod x = 0;;

let remove_multiple_of n l =
  filter_out (fun nb -> is_multiple nb n) l;;

let sieve max =
  let l = interval 2 max in
    let rec loop l =
      match l with
      |hd::tl -> if (hd * hd) <= max then
          hd::(loop (remove_multiple_of hd tl))
        else
          l
      |_-> l
  in loop l;;

(*print_int_list (sieve (int_of_string Sys.argv.(1)));;*)

let rec for_all f l =
  match l with
  |[] -> true
  | hd:: tl -> if f hd then
      for_all f tl
    else
      false;;

let map2 f l1 l2 =
  let rec loop f l1 l2 acc =
    if (l1 = []) || (l2 = [])then
      List.rev acc
    else
      loop f (List.tl l1) (List.tl l2) ((f (List.hd l1) (List.hd l2))::acc)
  in loop f l1 l2 []

let combine l1 l2 =
  let rec loop l1 l2 acc =
    if (l1 = []) || (l2 = [])then
      List.rev acc
    else
      loop (List.tl l1) (List.tl l2) (((List.hd l1), (List.hd l2))::acc)
  in loop l1 l2 []
