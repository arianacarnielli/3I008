type cell = Vivante|Morte
type monde = cell array array
type position = int * int

let test = [|
    [|Morte; Vivante; Morte|];
    [|Morte; Vivante; Morte|];
    [|Morte; Vivante; Morte|]
    |]

let overdone = [|
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte|];
  [|Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Vivante;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Vivante;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Vivante;Vivante;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|];
  [|Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte;Morte|]
|];;



let affiche_monde m =
  for i = 0 to (Array.length m -1) do
    let n = m.(i) in
    for j = 0 to (Array.length n - 1) do
      let x = n.(j) in
      if x = Vivante then
        Printf.printf "O "
      else
        Printf.printf "X "
    done;
    Printf.printf "\n"
  done

let est_vivante m pos =
  match pos with
  |i, j -> m.(i).(j) = Vivante

let est_grille m pos =
  match pos with
  |i, j ->
    let taille = Array.length m in
    (i >= 0) && (j >= 0) && (i < taille) && (j < taille)

let neighbours m pos =
  match pos with
  |i, j ->
    let viz = [(i + 1, j - 1); (i + 1, j); (i + 1, j + 1);
               (i, j - 1);                 (i, j + 1);
               (i - 1, j - 1); (i - 1, j); (i - 1, j + 1)] in
    let rec loop v acc =
      match v with
      |[] -> acc
      |hd::tl -> if est_grille m hd && est_vivante m hd then
          loop tl (acc + 1)
        else
          loop tl acc
    in loop viz 0

let next_gen m =
  let res = Array.make_matrix (Array.length m) (Array.length m) Morte in
  for i = 0 to (Array.length m -1) do
    let n = m.(i) in
    for j = 0 to (Array.length n - 1) do
      match neighbours m (i,j) with
      | 0 | 1 -> res.(i).(j) <- Morte
      |2 -> res.(i).(j) <- m.(i).(j)
      |3 -> res.(i).(j) <- Vivante
      |_ -> res.(i).(j) <- Morte
    done
  done;
  res

let next m =
  Array.mapi (fun i ligne ->
    Array.mapi (fun j elem ->
      match neighbours m (i,j) with
      | 0 | 1 -> Morte
      |2 -> elem
      |3 -> Vivante
      |_ -> Morte
    ) ligne
  ) m

let init_graph ?(size_cell = 5) m =
  let () = Graphics.open_graph "" in
  let () = Graphics.resize_window (size_cell * Array.length m) (size_cell * Array.length m) in
  let () = Graphics.set_window_title "Game of Life" in ()
  (*let () = Graphics.fill_rect 0 0 size_cell size_cell in ()*)

let draw_gen ?(size_cell = 5) m =
  Array.iteri (fun i ligne ->
    Array.iteri (fun j elem ->
          if m.(i).(j) = Vivante then
            Graphics.fill_rect (i*size_cell) (j*size_cell) size_cell size_cell
    ) ligne
  ) m

let continue () =
let tape = read_line () in
  match tape with
  |"q" -> false
  |_ -> true

let main test =
  let draw = ref test in
  let () = init_graph test in
  while continue () do
    Graphics.clear_graph ();
    draw_gen !draw;
    draw := next_gen !draw
  done;
    Graphics.close_graph ()




  (**)
