(** ptitexo 15 **)

[@@@warning "-8"]

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

let chemin (a : arbre) (x : int) : int list option =
  if nb_occurrences a x = 0 then None
  else
    let rec aux (a : arbre) (x : int) (l : int list option) =
      match a with
      | Vide -> None
      | Noeud (y, ls) -> (
          if y = x then l
          else
            match l with
            | None -> None
            | Some p -> aux_arbres ls x (Some (y :: p)))
    and aux_arbres (al : arbre list) (x : int) (l : int list option) =
      match al with
      | [] -> None
      | t :: ts -> (
          match aux t x l with Some p -> Some p | None -> aux_arbres ts x l)
    in
    let f = aux a x (Some []) in
    match f with None -> None | Some l -> Some (List.rev (x :: l))

(**let chemins (a : arbre) (x : int) : int list list = if nb_occurrences a x < 2
   then match (chemin a x) with | None -> [[]] | Some p -> [p] else**)

let rec feuilles (a : arbre) : int list =
  match a with
  | Vide -> []
  | Noeud (x, []) -> [ x ]
  | Noeud (x, arbs) -> autres_feuilles arbs

and autres_feuilles (arbs : arbre list) =
  match arbs with [] -> [] | x :: xs -> feuilles x @ autres_feuilles xs

let rec parcours_prefixe (a : arbre) =
  match a with
  | Vide -> []
  | Noeud (x, arbs) -> x :: autres_arbres_prefixes arbs

and autres_arbres_prefixes (a : arbre list) =
  match a with
  | [] -> []
  | x :: xs -> parcours_prefixe x @ autres_arbres_prefixes xs

let rec parcours_postfixe (a : arbre) =
  match a with
  | Vide -> []
  | Noeud (x, xs) -> autres_arbres_postfixes xs @ [ x ]

and autres_arbres_postfixes (arbs : arbre list) =
  match arbs with
  | [] -> []
  | x :: xs -> parcours_postfixe x @ autres_arbres_postfixes xs

let etiquettes_niveau (a : arbre) (level : int) =
  let rec aux (level : int) (a : arbre) : int list =
    if level = 0 then match a with Vide -> [] | Noeud (x, _) -> [ x ]
    else
      match a with
      | Noeud (_, xs) -> List.concat (List.map (aux (level - 1)) xs)
      | Vide -> []
  in
  aux level a

let est_uniforme (a : arbre) : bool =
  if a = Vide then true
  else
    let reference =
      let (Noeud (_, xs)) = a in
      List.length xs
    in
    let rec aux (a : arbre) : bool =
      match a with
      | Vide -> true
      | Noeud (_, []) -> true
      | Noeud (_, xs) ->
          if List.length xs <> reference then false
          else true && est_uniformes_autres xs
    and est_uniformes_autres (a : arbre list) : bool =
      match a with [] -> true | x :: xs -> aux x && est_uniformes_autres xs
    in
    aux a

let rec degre_max (a : arbre) : int =
  match a with
  | Vide -> 0
  | Noeud (_, xs) -> max (List.length xs) (autres_degre_max xs)

and autres_degre_max (a : arbre list) : int =
  match a with [] -> 0 | x :: xs -> max (degre_max x) (autres_degre_max xs)

let rec sous_arbre (a : arbre) (x : int) : arbre option =
  if nb_occurrences a x = 0 then None
  else
    let (Some path) = chemin a x in
    let rec aux (a : arbre) (path : int list) =
      match (a, path) with
      | Vide, _ -> None
      | Noeud (_, _), [] -> Some a
      | Noeud (x, arbs), z :: [] -> if x <> z then None else Some a
      | Noeud (x, arbs), z :: zs -> if x <> z then None else autres_sb arbs zs
    and autres_sb (arbs : arbre list) (path : int list) =
      match arbs with
      | [] -> None
      | x :: xs -> if aux x path = None then autres_sb xs path else aux x path
    in
    aux a path

let ppac (a : arbre) (n1 : int) (n2 : int) : int option =
  let p1 = chemin a n1 in
  let p2 = chemin a n2 in
  match (p1, p2) with
  | None, None -> None
  | None, _ -> None
  | _, None -> None
  | Some pt1, Some pt2 ->
      let l1 = pt1 in
      let l2 = pt2 in
      let rec aux (l1 : int list) (l2 : int list) (ans : int option) =
        match (l1, l2) with
        | x :: xs, y :: ys -> if x = y then aux xs ys (Some x) else ans
        | _, [] -> ans
        | [], _ -> ans
      in
      aux l1 l2 None
