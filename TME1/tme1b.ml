let rec for_all f l =
  match l with
  | [] -> true
  | h :: t -> (f h) && (for_all f t);;

let map2 f l1 l2 =
  let rec loop f l1 l2 accu =
    if (l1 = []) || (l2 = []) then
      accu
    else begin
      accu @ (f (List.hd l1) (List.hd l2)) :: (loop f (List.tl l1) (List.tl l2) accu)
    end
  in (loop f l1 l2 []);;

let combine l1 l2 =
  let rec loop l1 l2 accu =
    if (l1 = []) || (l2 = []) then
      accu
    else begin
      accu @ ((List.hd l1), (List.hd l2)) :: (loop (List.tl l1) (List.tl l2) accu)
    end
  in (loop l1 l2 []);;


let l1 = [1;2;3];;
let l2 = [0;2;3;9;4];;
let l3 = ["a"; "b"; "c"]
