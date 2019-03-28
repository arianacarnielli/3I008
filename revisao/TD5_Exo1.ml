type 'a arbre =
  |Empty
  |Noeud of 'a * 'a arbre * 'a arbre

module type S =
  sig
    type key
    val create : unit -> (key * 'a) arbre
    val add : (key * 'a) -> (key * 'a) arbre -> (key * 'a) arbre
    val find : key -> (key * 'a) arbre -> 'a
  end

module type Ordered =
  sig
    type t
    val compare : t -> t -> int
  end

module Make (Ord: Ordered): (S with type key = Ord.t) =
  struct
    type key = Ord.t

    let create () = Empty

    let rec add (key, valeur) arbre =
      match arbre with
      |Empty -> Noeud((key, valeur), Empty, Empty)
      |Noeud((k, v), g, d) ->
        if Ord.compare k key < 0 then
          Noeud((k,v), g, (add (key, valeur) d))
        else
        if Ord.compare k key > 0 then
          Noeud((k,v), (add (key, valeur) g), d)
        else
          Noeud((k,valeur), g, d)

    let rec find key arbre =
      match arbre with
      |Empty -> raise Not_found
      |Noeud((k, v), g, d) ->
        if Ord.compare k key  = 0 then
          v
        else if Ord.compare k key > 0 then
          find key g
        else
          find key d

  end
