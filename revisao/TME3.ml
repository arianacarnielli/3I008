(*Exercice 4*)
type arbre_lex = noeud list and noeud = Lettre of char * bool * arbre_lex

exception Deja_defini

let rec existe dico mot =
  match dico with
  |[] -> false
  |hd::tl ->
    match hd with
    |Lettre(c, b, ab)->
      if ((c = mot.[0]) && ((String.length mot) = 1)) then
        b
      else
        if (c <> mot.[0])then
          existe tl mot
        else
          existe ab (String.sub mot 1 ((String.length mot) - 1))

let ajoute dico mot =
  let rec aux dico i =
    match dico with
    |[] ->
      if i = (String.length mot - 1) then
        [Lettre(mot.[i], true, [])]
      else
        [Lettre(mot.[i], false, aux [] (i+1))]
    |hd::tl ->
      match hd with
      |Lettre(c, b, l_ab) ->
        if c = mot.[i] then
          if i = (String.length mot - 1) then
            if b = true then
              raise Deja_defini
            else
              Lettre(c, true, l_ab)::tl
          else
            Lettre(c, b, (aux l_ab (i+1)))::tl
        else
          hd::(aux tl i)
  in aux dico 0

let construit l_mot =
  let rec aux l_mot acc =
    match l_mot with
    |[] -> acc
    |hd::tl ->
      try aux tl (ajoute acc hd) with
      Deja_defini -> aux tl acc
  in aux l_mot []

let liste_de_dict dico =
  let rec aux dico str acc =
    match dico with
    |[] -> acc
    |hd::tl ->
      match hd with
      |Lettre(c, b, l_ab) ->
        let str2 = str^(Char.escaped c) in
        let acc2 = if b then str2::acc else acc in
        aux l_ab str2 acc2@(aux tl str [])
  in aux dico "" []

let affiche dico =
  let l_mots = liste_de_dict dico in
  List.iter print_endline l_mots;;

(* affichage programme
let _ = print_endline "Bienvenue !" in
let reg = Str.regexp "^\\([A-Za-z]+\\|\\([A-Za-z]+\\) \\([A-Za-z]+\\)\\)$" in
let mots = ref [] in
let bol = ref true in
  while !bol do
    let _ = print_string "> " in
    let tape = read_line () in
    if Str.string_match reg tape 0 then
      let s = Str.matched_group 1 tape in
      match s with
      |"quitte" -> bol:= false
      |"affiche" -> affiche !mots
      |_ ->
        try let s2 = Str.matched_group 2 tape in
          match s2 with
          |"ajoute" -> mots:= ajoute !mots (Str.matched_group 3 tape)
          |"existe" -> Printf.printf "%B\n" (existe !mots (Str.matched_group 3 tape))
          |_ -> print_endline "commande invalide"
        with
        |Deja_defini -> print_endline "mot deja defini"
        |Not_found -> print_endline "commande invalide"
    else
      print_endline "commande invalide"
  done
*)

(* tests exercice 4
let lettre_i = Lettre('i', true, [])
let lettre_l = Lettre('l', true, [])
let lettre_s = Lettre('s', true, [])
let lettre_r = Lettre('r', true, [])

let lettre_o = Lettre('o', true, [lettre_i; lettre_l])
let lettre_a = Lettre('a', false, [lettre_s; lettre_r])
let lettre_m = Lettre('m', false, [lettre_o])
let lettre_p = Lettre('p', false, [lettre_a])

let racine = [lettre_m; lettre_p]

let liste_mot = ["teste";"tes";"tem";"teto";"teto";"caro";"cara";"carta"]
*)

(*Exercice 5*)
exception Empty_queue

type 'a queue = {debut: 'a list; fin: 'a list}

let create () = {debut = []; fin = []}

let push elem q = {debut = q.debut; fin = (elem::q.fin)}

let pop q =
  match q.debut with
  |hd::tl -> (hd, {debut = tl; fin = q.fin})
  |[] -> if q.fin = [] then
      raise Empty_queue
    else
      let deb = List.rev q.fin in
      (List.hd deb, {debut = List.tl deb; fin = []})

let to_list q =
  let rec loop q acc =
    try
      match pop q with
      |v, q -> loop q (v::acc)
    with
      Empty_queue -> List.rev acc
  in loop q []
