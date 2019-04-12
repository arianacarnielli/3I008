type operator = (* à compléter *)

type state = (* à compléter *)

let create () = failwith "Not implemented" (* *)

let add_digit state digit = failwith "Not implemented"  (* *)

let apply_equal state = failwith "Not implemented"  (* *)

let apply_op state operator = failwith "Not implemented"  (* *)

(* Tests *)

type input =
  | N
  | D of int
  | O of operator
  | E
    
let next_state state = function
  | N -> state
  | D d -> add_digit state d
  | O o -> apply_op state o
  | E -> apply_equal state
           
let rec compute state = function
  | [] -> [state, N]
  | x :: xs -> (state, x) :: compute (next_state state x) xs
                 
let doit xs = compute (create ()) xs
    
let rec extract_result = function
  | [] -> 0.
  | [{x; _},_ ] -> x
  | _ :: tl -> extract_result tl
                 
let check expected seq =
  let c = doit seq in
  let r = extract_result c in
  Printf.printf "%g = %g: %b\n%!" expected r (expected = r)
  
let () = 
  (* séquence de touches: 1 + 2 + 3 = *)
  check 6. [D 1; O Plus; D 2; O Plus; D 3; E];
  (* séquence de touches: 1 + 2 * 3 = *)
  check 9. [D 1; O Plus; D 2; O Mult; D 3; E];
  (* séquence de touches: 2 0 + 4 - 3 * 2 = *)
  check 42. [D 2; D 0; O Plus; D 4; O Minus; D 3; O Mult; D 2; E];
  (* séquence de touches: 1 + 2 = *)
  check 3. [D 1; O Plus; D 2; E];
  (* séquence de touches: 1 + 2 = 3 + 4 = *)
  check 7. [D 1; O Plus; D 2; E; D 3; O Plus; D 4; E];
  (* séquence de touches: 2 * = = = = *)
  check 32. [D 2; O Mult; E; E; E; E];
  (* séquence de touches: 1 - 2 = = = *)
  check (-5.) [D 1; O Minus; D 2; E; E; E];
  (* séquence de touches: 1 + = = = = *)
  check 5. [D 1; O Plus; E; E; E; E];
  (* séquence de touches: 1 + 2 = 5 = *)
  check 7. [D 1; O Plus; D 2; E; D 5; E]