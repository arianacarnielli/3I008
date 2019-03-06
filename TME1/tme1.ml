let rec print_int_list l =
  match l with
  | [] -> ()
  | h :: t -> print_int(h); print_int_list(t)

let test_list = [1;2;3;4;5];;

(* print_int_list test_list;; *)

let rec interval n m =
  if n = m then
    [m]
  else begin
    if n < m then
      n :: (interval (n + 1) m)
    else
      []
  end;;

(* print_int_list (interval 2 10);; *)

let rec filter_out p l =
  match l with
  | [] -> []
  | h :: t -> if not (p h) then
      h :: filter_out p t
    else
      filter_out p t

let is_multiple m x =
  if (m mod x) = 0 then
    true
  else
    false

let remove_multiple_of n l =
  filter_out (fun m -> is_multiple m n) l

let sieve maxi =
  let li = (interval 2 maxi) in
  let rec loop l =
    match l with
    | [] -> []
    | h :: t -> if (h * h) > maxi then
        l
      else
        h :: (loop(remove_multiple_of h t))
 in loop li;;

let paire x =
  if x mod 2 = 0 then
    true
  else
    false;;

print_int_list (sieve (int_of_string Sys.argv.(1)));;
