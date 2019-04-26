(******************************************************************************)
(* orush_js.ml                                                                *)
(******************************************************************************)

open Dom

(* val set_grid : Dom.Canvas.RenderingContext2D.t -> unit *)
(* Dessine les lignes en blanc d'un grid 6 x 6 dans le canvas dont le
   RenderingContext2D est passé en argument. *)
let set_grid ctx_grid =
  let _ = Canvas.RenderingContext2D.set_stroke_style ctx_grid "white" in
  let _ = Canvas.RenderingContext2D.beginPath ctx_grid in
  let rec loop i =
    if i < 6 then
      let _ = Canvas.RenderingContext2D.move_to ctx_grid (60*i) 0 in
      let _ = Canvas.RenderingContext2D.line_to ctx_grid (60*i) 360 in
      let _ = Canvas.RenderingContext2D.move_to ctx_grid 0 (60*i) in
      let _ = Canvas.RenderingContext2D.line_to ctx_grid 360 (60*i) in
      loop (i+1)
  in loop 1;
  let _ = Canvas.RenderingContext2D.stroke ctx_grid in
  let _ = Canvas.RenderingContext2D.set_fill_style ctx_grid "black" in
  Canvas.RenderingContext2D.fill_rect ctx_grid 355 (60*2) 5 60

(* val draw_boat : Port.boat -> Dom.Canvas.RenderingContext2D.t -> unit *)
(* Prend en argument un bateau et un canvas et dessine ce bateau dans le
   canvas. *)
let draw_boat boat ctx_grid =
  let _ = Canvas.RenderingContext2D.set_fill_style ctx_grid Maps.colors.((int_of_char (Port.string_of_boat boat).[0]) - 65) in
  let taille = Port.taille_of_boat boat in
  match Port.x_y_of_boat boat with
  |(x, y) ->
    if Port.pos_of_boat boat = 'H' then
      Canvas.RenderingContext2D.fill_rect ctx_grid (x*60 + 10) (y*60 + 10) (taille*60 - 20) 40
    else
      Canvas.RenderingContext2D.fill_rect ctx_grid (x*60 + 10) (y*60 + 10) 40 (taille*60 - 20)

(* val draw_state : Port.boat list -> Dom.Canvas.RenderingContext2D.t -> unit *)
(* Prend en argument un état et un canvas et dessine les bateaux de cet état
   dans le canvas. *)
let draw_state state ctx_grid =
  let rec loop st cpt =
    match st with
    |[] -> ()
    |hd::tl ->
      draw_boat hd ctx_grid;
      loop tl (cpt + 1)
  in loop state 0

(* val add_maps : Dom.Element.t -> unit *)
(* Ajoute toutes les cartes du module Maps dans le menu. *)
let add_maps btn_map =
  Array.iteri (
    fun i map ->
      let opt = Document.create_element document "option" in
      let _ = Element.set_attribute opt "value" (string_of_int i) in
      let _ = Element.set_text_content opt ("Carte "^(string_of_int (i+1))) in
      Element.append_child btn_map opt
  ) Maps.all_maps

(* val clear_canvas : Dom.Canvas.RenderingContext2D.t -> unit *)
(* Efface tous les éléments du canvas. *)
let clear_canvas ctx_grid =
  Canvas.RenderingContext2D.clear_rect ctx_grid 0 0 360 360

(* val boat_of_click : Dom.Event.t -> char ref -> Port.boat list ref -> unit *)
(* Capture un click sur le canvas et modifie la variable boat_courant pour
   qu'elle contienne le nom du bateau dans la zone clickée. *)
let boat_of_click e boat_courant state_courant =
  let x = (Event.offset_x e)/60 in
  let y = (Event.offset_y e)/60 in
  let grid = Port.grid_of_state !state_courant in
  boat_courant := grid.(y).(x)

(* val start :
   Dom.Canvas.RenderingContext2D.t ->
   Dom.Element.t ->
   Dom.Element.t list ->
   Dom.Element.t ->
   int ref -> Port.boat list ref -> char ref -> int ref -> 'a list ref -> unit *)
(* Demarre le jeu avec la carte sélectionnée. *)
let start ctx_grid btn_step btn_list solution_txt carte_courant state_courant boat_courant cpt_moves solution =
  let _ = clear_canvas ctx_grid in
  let _ = set_grid ctx_grid in
  let menu = Document.get_element_by_id document "map" in
  let i = int_of_string (Element.value menu) in
  let state = Maps.all_maps.(i) in
  let _ = (state_courant := state) in
  let _ = (boat_courant := 'A') in
  let _ = (cpt_moves := 0) in
  let _ = (solution := []) in
  let _ = Element.set_attribute btn_step "style" "visibility: hidden" in
  let _ = List.iter (fun btn -> Element.remove_attribute btn "disabled") btn_list in
  let _ =   draw_state state ctx_grid in
  if !carte_courant <> i then
    let _ = (carte_courant := i) in
    Element.set_text_content solution_txt ""

(* val move_of_click :
   Dom.Canvas.RenderingContext2D.t ->
   Port.boat list ref -> char ref -> int ref -> string -> unit *)
(* Fait bouger un bateau à partir d'un click sur bouton de mouvement ou
   une touche du clavier. *)
let move_of_click ctx_grid state_courant boat_courant cpt_moves btn_name =
  if !boat_courant <> '~' then
    let move = Moves.move_of_string ((Char.escaped !boat_courant)^
      match btn_name with
      | "left" | "up" -> "<"
      | _ -> ">" ) in
    let boat = Port.get_boat !boat_courant !state_courant in
    if (Port.pos_of_boat boat = 'H' && (btn_name = "left" || btn_name = "right")) ||
       (Port.pos_of_boat boat = 'V' && (btn_name = "up" || btn_name = "down")) then
      try
        let new_state = Moves.apply_move move !state_courant in
        let _ = (state_courant := new_state) in
        let _ = (cpt_moves := !cpt_moves + 1) in
        let _ = clear_canvas ctx_grid in
        let _ = set_grid ctx_grid in
        let _ = draw_state !state_courant ctx_grid in
        if Moves.win !state_courant then
          Window.alert window ("Gagné en "^(string_of_int !cpt_moves)^" pas !")
      with
      | _ -> ()

(* val solve_start :
   Port.boat list ref ->
   Dom.Element.t ->
   Dom.Element.t ->
   Dom.Element.t list -> int ref -> Moves.move list ref -> unit *)
(* Calcul de la solution d'une grille et mise à jour de la variable contenant
   la solution. *)
let solve_start state_courant text btn_step list_btn cpt_moves solution =
  if Moves.win !state_courant then
    Window.alert window ("Gagné en "^(string_of_int !cpt_moves)^" pas !")
  else
    let _ = Element.set_attribute btn_step "style" "visibility: visible" in
    let solution_str = Solver.solve_state !state_courant in
    let _ = (solution := Moves.list_move_of_string solution_str) in
    let _ = List.iter (fun btn -> Element.set_attribute btn "disabled" "true") list_btn in
    Element.set_text_content text solution_str

(* val solve_step :
   Dom.Canvas.RenderingContext2D.t ->
   Port.boat list ref ->
   Dom.Element.t ->
   Dom.Element.t list -> int ref -> Moves.move list ref -> unit *)
(* Fait un pas de la résolution dans la variable "solution". *)
let solve_step ctx_grid state_courant btn_step list_btn cpt_moves solution =
  match !solution with
  |hd::tl ->
    let _ = (state_courant := Moves.apply_move hd !state_courant) in
    let _ = (clear_canvas ctx_grid) in
    let _ = (set_grid ctx_grid) in
    let _ = (draw_state !state_courant ctx_grid) in
    let _ = (solution := tl) in
    let _ = (cpt_moves := !cpt_moves + 1) in
    if !solution = [] then
      let _ = Element.set_attribute btn_step "style" "visibility: hidden" in
      let _ = List.iter (fun btn -> Element.remove_attribute btn "disabled") list_btn in
      Window.alert window ("Gagné en "^(string_of_int !cpt_moves)^" pas !")
  |_ -> ()

(* val main : unit -> unit *)
(* Fonction principale. *)
let main () =
  (* Variables contenant l'état du jeu. *)
  let carte_courant = ref 0 in
  let state_courant = ref [] in
  let boat_courant = ref '~' in
  let cpt_moves = ref 0 in
  let solution = ref [] in

  (*set title*)
  let _ = Document.set_title document "ORush" in
  let title = Document.get_element_by_id document "title" in
  let _ = Element.set_text_content title "ORush" in

  (*set canvas*)
  let orush = Document.get_element_by_id document "canvas" in
  let _ = Element.set_attribute orush "width" "360" in
  let _ = Element.set_attribute orush "height" "360" in
  let _ = Element.set_attribute orush "style" "background: url('water.jpg'); background-size: contain" in

  let grid = Canvas.of_element orush in
  let ctx_grid = Canvas.get_context_2d grid in

  (*set buttons*)
  let body = Document.get_element_by_id document "body" in

  let div_1 = Document.create_element document "div" in
  let _ = Element.append_child body div_1 in

  let btn_left = Document.create_element document "button" in
  let _ = Element.set_attribute btn_left "id" "left" in
  let _ = Element.set_text_content btn_left "←" in
  let _ = Element.append_child div_1 btn_left in

  let btn_up = Document.create_element document "button" in
  let _ = Element.set_attribute btn_up "id" "up" in
  let _ = Element.set_text_content btn_up "↑" in
  let _ = Element.append_child div_1 btn_up in

  let btn_right = Document.create_element document "button" in
  let _ = Element.set_attribute btn_right "id" "right" in
  let _ = Element.set_text_content btn_right "→" in
  let _ = Element.append_child div_1 btn_right in

  let btn_down = Document.create_element document "button" in
  let _ = Element.set_attribute btn_down "id" "down" in
  let _ = Element.set_text_content btn_down "↓" in
  let _ = Element.append_child div_1 btn_down in

  let div_2 = Document.create_element document "div" in
  let _ = Element.append_child body div_2 in

  let btn_map = Document.create_element document "select" in
  let _ = Element.set_attribute btn_map "id" "map" in
  let _ = Element.append_child div_2 btn_map in
  let _ = add_maps btn_map in

  let btn_start = Document.create_element document "button" in
  let _ = Element.set_attribute btn_start "id" "start" in
  let _ = Element.set_text_content btn_start "start" in
  let _ = Element.append_child div_2 btn_start in

  let btn_solve = Document.create_element document "button" in
  let _ = Element.set_attribute btn_solve "id" "solve" in
  let _ = Element.set_text_content btn_solve "solve" in
  let _ = Element.append_child div_2 btn_solve in

  let btn_step = Document.create_element document "button" in
  let _ = Element.set_attribute btn_step "id" "step" in
  let _ = Element.set_text_content btn_step "next step" in
  let _ = Element.set_attribute btn_step "style" "visibility: hidden" in
  let _ = Element.append_child div_2 btn_step in

  (*set texts*)
  let div_3 = Document.create_element document "div" in
  let _ = Element.append_child body div_3 in
  let text = Document.create_text_node document "" in
  let _ = Element.append_child div_3 text in

  let div_4 = Document.create_element document "div" in
  let _ = Element.append_child body div_4 in
  let solution_txt = Document.create_text_node document "" in
  let _ = Element.append_child div_4 solution_txt in

  let btn_list = [btn_left; btn_right; btn_up; btn_down; btn_solve] in

  (*set events*)
  let _ = Window.add_event_listener window "keydown" (
      fun e ->
        let key_name =
          match Event.which e with
          | 37 -> "left"
          | 38 -> "up"
          | 39 -> "right"
          | 40 -> "down"
          | _ -> ""
        in
        if key_name <> "" then
          move_of_click ctx_grid state_courant boat_courant cpt_moves key_name; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")
    ) false in

  let _ = Element.add_event_listener btn_start "click" (fun e -> start ctx_grid btn_step btn_list solution_txt carte_courant state_courant boat_courant cpt_moves solution; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  let _ = Element.add_event_listener btn_left "click" (fun e -> move_of_click ctx_grid state_courant boat_courant cpt_moves "left"; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  let _ = Element.add_event_listener btn_right "click" (fun e -> move_of_click ctx_grid state_courant boat_courant cpt_moves "right"; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  let _ = Element.add_event_listener btn_up "click" (fun e -> move_of_click ctx_grid state_courant boat_courant cpt_moves "up"; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  let _ = Element.add_event_listener btn_down "click" (fun e -> move_of_click ctx_grid state_courant boat_courant cpt_moves "down"; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  let _ = Element.add_event_listener orush "click" (fun e -> boat_of_click e boat_courant state_courant) false in

  let _ = Element.add_event_listener btn_step "click" (fun e -> solve_step ctx_grid state_courant btn_step btn_list cpt_moves solution; Element.set_text_content text ((string_of_int !cpt_moves)^" pas effectués")) false in

  Element.add_event_listener btn_solve "click" (fun e -> solve_start state_courant solution_txt btn_step btn_list cpt_moves solution) false

let _ = main ()
