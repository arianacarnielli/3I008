type cell =
  |Vivante
  |Morte
;;

type monde = cell array array;;

let init_gen = [|[|Morte; Vivante; Morte|];
                 [|Morte; Morte; Vivante|];
                 [|Morte; Vivante; Morte|]
               |];;

let affiche_monde md =
  Array.iteri (
    fun i a ->
      Array.iteri (
        fun j x ->
          if (x = Morte) then
            Printf.printf " x "
          else
            Printf.printf " o "
      ) a;
      Printf.printf "\n"
  ) md;;

let est_partie taille (i,j) =
  ((i >= 0) && (i < taille)) && ((j >= 0) && (j < taille));;

let est_vivante md (i, j) =
  let ligne = md.(i) in
    let elem = ligne.(j) in
      elem = Vivante;;

let neighbours md (i, j) =
  let l = [(i-1, j-1); (i-1, j); (i-1, j+1); (i, j-1); (i, j+1); (i+1, j-1); (i+1, j); (i+1, j+1)] in
    let l = List.filter (est_partie (Array.length md)) l in
      let elem = List.filter (est_vivante md) l in
  List.length elem;;

(*nao deveria ter que usar isso*)
let copie_monde md =
  let new_md = ref [||] in
    for i = 0 to Array.length md - 1 do
      new_md := Array.append !new_md (Array.copy md.(i))
    done;
!new_md;;

let next_gen md =
  Array.mapi (
    fun i ligne ->
      Array.mapi (
        fun j elem ->
          if neighbours md (i, j) < 2 || neighbours md (i, j) > 3 then
            Morte
          else
            Vivante
      )ligne;
  )md;;
