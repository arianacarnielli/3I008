(*Exo 6*)
let calcul_prefixe l =
  let rec loop l nb acc =
    match l with
    | [] -> acc
    | hd :: tl -> if nb = hd then
       loop tl nb (acc + 1)
      else
        acc
  in loop l (List.hd l) 0;;

let genere_liste l =
  let res =
    let rec loop l nb v acc =
      match l with
      |[] -> nb::v::acc
      |hd :: tl -> if hd = nb then
          loop tl nb v acc
        else
          loop tl hd (calcul_prefixe l) (nb::v::acc)
    in loop l (List.hd l) (calcul_prefixe l) []
  in List.rev res
;;

let genere k =
  let rec loop k acc =
    match k with
    |0 -> acc
    |_ -> loop (k-1) (genere_liste acc)
  in loop (k-1) [1]
;;

let l = [1;1;1;2;2;1];;

(*Exo 7*)
type couleur =
  | Pique
  | Coeur
  | Carreau
  | Trefle;;

type carte =
  |As of couleur
  |Roi of couleur
  |Dame of couleur
  |Valet of couleur
  |Autre of couleur * int;;

let valeur atout carte =
  match carte with
  |As _ -> 11
  |Roi _ -> 4
  |Dame _ -> 3
  |Valet c -> if c = atout then 20
    else 2
  |Autre (_, 10) -> 10
  |Autre (c, 9) -> if c = atout then 14 else 0
  |_ -> 0
;;

let valeur_jeu atout lc =
  let rec loop atout lc acc =
    match lc with
    | [] -> acc
    |hd :: tl -> loop atout tl (acc + valeur atout hd)
  in loop atout lc 0
;;

let c = Autre(Trefle, 9);;
let at = Trefle;;
let test = [Valet Carreau; Autre(Trefle, 10); Autre(Coeur, 7); Autre(Carreau, 8); Autre(Pique, 9)];;

(*Exo 8*)
type utilisateur = {
  nom: string;
  amis: (string)list;
}

type cible =
  |ToutMonde
  |Amis
  |ListeAmisSpe;;

type publication = {
  cible: cible;
  auteur: string;
  message: string;
}

let utilisateur ~nom:n ~amis:a = {nom = n; amis = a};;

let publication ?cible:(c = Amis) ~auteur:a msg = {cible = c; auteur = a; message = msg};;
