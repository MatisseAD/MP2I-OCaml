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

(**let searchObjectinABR (t : 'a arbre) (x : 'a) = let rec aux t ans = match t
   with | Vide -> aux t ans | N (g,p,d) -> if p = x then**)

(** Exercice 1 TD *)

type arbre = Vide | Noeud of int * arbre list

let rec taille (t : arbre) : int =
  match t with Vide -> 0 | Noeud (_, l) -> 1 + autres_arbres l

and autres_arbres l =
  match l with [] -> 0 | x :: xs -> taille x + autres_arbres xs

let t =
  Noeud
    ( 1,
      [
        Noeud (2, []); Noeud (3, [ Noeud (5, []); Noeud (6, []) ]); Noeud (4, []);
      ] )

let rec hauteur (t : arbre) : int =
  match t with Vide -> -1 | Noeud (_, l) -> 1 + hauteurs_arbres l

and hauteurs_arbres l =
  match l with [] -> -1 | x :: xs -> max (hauteur x) (hauteurs_arbres xs)

let rec contient (a : arbre) (x : int) : bool =
  match a with
  | Vide -> false
  | Noeud (p, l) -> if p = x then true else false || autres_arbres_content l x

and autres_arbres_content l x =
  match l with
  | [] -> false
  | ar1 :: ar1s -> contient ar1 x || autres_arbres_content ar1s x

let nb_occurrences (a : arbre) (x : int) : int =
  let rec aux (a : arbre list) (x : int) (cpt : int) : int =
    match a with
    | [] -> cpt
    | t :: ts -> (
        match t with
        | Vide -> aux ts x cpt
        | Noeud (p, l) ->
            if p = x then aux (l @ ts) x (cpt + 1) else aux (l @ ts) x cpt)
  in
  aux [ a ] x 0

let chemin (a : arbre) (x : int) =
  let rec aux (a : arbre) (x : int) (l : int list option) =
    