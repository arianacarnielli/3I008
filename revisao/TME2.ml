(*Exercice 7*)
type couleur =
  |Carreau
  |Coeur
  |Pique
  |Trefle

type carte =
  |As of couleur
  |Roi of couleur
  |Dame of couleur
  |Valet of couleur
  |Autres of couleur * int

let valeur atout carte =
  match carte with
  |As _ -> 11
  |Roi _ -> 4
  |Dame _ -> 3
  |Valet col -> if col = atout then
      20
    else
      2
  |Autres (_, 10) -> 10
  |Autres (col, 9) -> if col = atout then
      14
    else
      0
  |_ -> 0

let valeur_jeu atout l =
  let rec loop atout l acc =
    match l with
    |[] -> acc
    |hd::tl -> loop atout tl (acc + (valeur atout hd))
  in loop atout l 0

let l = [Valet Carreau; Autres(Trefle, 10); Autres(Coeur, 7); Autres(Carreau, 8); Autres(Pique, 9)];;

(*Exercice 6*)

let calcul_prefixe l =
  if l = [] then
    0
  else
    let rec loop l nb acc =
      if (l <> []) && (List.hd l) = nb then
        loop (List.tl l) nb (acc+1)
      else
        acc
    in loop l (List.hd l) 0

let genere_liste l =
  let rec loop l cpt acc =
    match l with
    | [] -> List.rev acc
    | hd::tl -> if cpt = 0 then
        let pre = calcul_prefixe l in
        loop tl (pre - 1) (hd::pre::acc)
      else
        loop tl (cpt-1) acc
  in loop l 0 []

let genere k =
  let rec loop k acc =
    if k = 1 then
      List.rev acc
    else
      loop (k-1) ((genere_liste (List.hd acc))::acc)
  in loop k [[1]]

(*Exercice 8*)

type utilisateur = {nom: string; amis: string list}

type cible =
  |Tous
  |Amis
  |List of string list

type publication = {auteur: string; msg: string; cible: cible}

let utilisateur ~nom:n ~amis:a =
  {nom = n; amis = a}

let publication ?(cible = Amis) ~auteur msg =
  {auteur = auteur; msg = msg; cible = cible}

let afficher_publication pub =
  print_string (pub.auteur^": "^pub.msg^"\n")

let acces_autorise pers pub =
  if pers.nom = pub.auteur then
    true
  else
    match pub.cible with
    |Tous -> true
    |List l -> List.mem pers.nom l
    |Amis -> List.mem pub.auteur pers.amis

let filtre_publications pers lpub =
  let rec loop pers lpub acc =
    match lpub with
    |[] -> List.rev acc
    |hd::tl -> if acces_autorise pers hd then
        loop pers tl (hd::acc)
      else
        loop pers tl acc
  in loop pers lpub []

let categoriser lpub =
  let rec loop lpub a p =
    match lpub with
    | [] -> (List.rev a, List.rev p)
    | hd::tl -> if hd.cible = Tous then
        loop tl a (hd::p)
      else
        loop tl (hd::a) p
  in loop lpub [] []

let u1 = utilisateur "user1" ["user2"; "user3"; "user4"];;
let u2 = utilisateur "user2" ["user1"; "user3"];;
let u3 = utilisateur "user3" ["user1"; "user2"; "user4"];;
let u4 = utilisateur "user4" ["user1"; "user3"];;

let p1 = publication "user1" "Hello my friends!";;
let p2 = publication ~cible:(List ["user1"]) ~auteur:"user2" "Hello from user2 to user1";;
let p3 = publication ~cible:Tous ~auteur:"user4" "Hello to everyone";;
let p4 = publication ~auteur:"user4" "Hello my dear friends";;

let l1 = [p1; p2; p3; p4];;

(*Exercice 9*)

let cherche_car car str =
  try String.index str car with
    Not_found -> (-1)

let modif_str str =
  let rec loop str cpt acc =
    if cpt < String.length str then
      match str.[cpt] with
      |',' -> loop str (cpt+1) (acc^" ,")
      |'.' -> loop str (cpt+1) (acc^" .")
      | _ -> loop str (cpt+1) (acc^(Char.escaped(str.[cpt])))
    else
      acc
  in loop str 0 ""

let s = "vai tomar no cu";;

let s2 = "Ei, voce! vai tomar no cu.";;
