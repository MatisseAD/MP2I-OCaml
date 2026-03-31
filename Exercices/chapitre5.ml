(** ptitexo 15 **)

type 'a arbre = Vide | N of 'a arbre * 'a * 'a arbre
type sens = Droite | Gauche

let rec getEtiquette (t : 'a arbre) (x : int) (y : sens) =
  match (t, y) with
  | N (_, k, d), Droite -> getEtiquette d k Droite
  | N (g, k, _), Gauche -> getEtiquette g k Gauche
  | Vide, Droite -> x + 1
  | Vide, Gauche -> x - 1

let rec verifABR (t : 'a arbre) : bool =
  match t with
  | Vide -> true
  | N (g, x, d) ->
      (getEtiquette g x Gauche <= x && x <= getEtiquette d x Droite)
      && verifABR g && verifABR d

(** Exercice 5.5 *)

let rec maxABR (t : 'a arbre) =
  match t with
  | Vide -> min_int
  | N (g, x, d) -> max (max x (maxABR g)) (max x (maxABR d))


(** Exercice 5.4 *)

let searchObjectinABR (t : 'a arbre) (x : 'a) =
  let rec aux t ans =
    match t with
    | Vide -> aux t ans
    | N (g,p,d) ->
      if p = x then
