type move = {nom: char; m: int}

let string_of_move m =
  let res = Bytes.create 2 in
  let () = Bytes.set res 0 m.nom in
  let () = Bytes.set res 1 (if m.m = -1 then '<' else '>') in
  Bytes.to_string res

let move_of_string str =
  {nom = str.[0]; m = (if str.[1] = '<' then -1 else 1)}

let apply_move m s =
  let new_s = Array.map () s in new_s



(* apply_move m s applique le mouvement m depuis l’etat s et retourne
le nouvel etat. Leve l’exception Cannot_move si le mouvement est
impossible *)

  (*[11:38, 3/29/2019] Guilherme: Era na de input, se ele vai ler arquivo (e você pára de ler com End_of_file) ou se ele vai ler de stdin e você pára com outro critério.
[11:38, 3/29/2019] Guilherme: E se o que ele vai ler já é o mapa 6×6 ou a lista de barcos.*)
