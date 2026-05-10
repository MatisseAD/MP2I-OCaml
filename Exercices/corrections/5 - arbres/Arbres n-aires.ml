(*
=========================================================
CORRIGÉ — ARBRES N-AIRES (OCaml)
Questions Q1 à Q14
=========================================================

Représentation :
Un arbre est soit vide, soit un nœud contenant une valeur entière
et une liste de sous-arbres.
*)

type arbre =
  | Vide
  | Noeud of int * arbre list;;
  
  
  
 (* Exemple*)
 
 let t =
  Noeud (1, [
    Noeud (2, []);
    Noeud (3, [
      Noeud (5, []);
      Noeud (6, [])
    ]);
    Noeud (4, [])
  ])


(*
---------------------------------------------------------
Q1 — Taille de l’arbre
---------------------------------------------------------
La taille est le nombre total de nœuds.
Récurrence structurelle sur l’arbre.
*)


let rec taille a = 
	let  somme_liste l  = 
	(* calcule la somme des éléments d'une liste l*)
		List.fold_left (fun acc x  -> acc + x) 0 l 
	in 
	match a with 
	Vide -> 0
	|Noeud (_, fils) -> 
	  1 + ( fils |> (List.map taille) |> somme_liste);;
	
	


let rec taille2 a =
  match a with
  | Vide -> 0
  | Noeud (_, fils) ->
      1 + List.fold_left (fun acc t -> acc + taille2 t) 0 fils;;

taille t;;
taille2 t;;

(*
---------------------------------------------------------
Q2 — Hauteur de l’arbre
---------------------------------------------------------
Convention classique :
- hauteur d’un arbre vide : -1
- hauteur d’une feuille : 0
*)

let rec hauteur a = 
	let  maxi_liste l  = 
	  (* calcule le max d'une liste d'éléments positifs*)
		List.fold_left max (-1) l 
	in 
	match a with 
	Vide -> -1
	|Noeud (_, fils) -> 
	  1 + ( fils |> (List.map hauteur) |> maxi_liste);;


let rec hauteur2 a =
  match a with
  | Vide -> -1
  | Noeud (_, fils) ->
      1 + List.fold_left
            (fun acc t -> max acc (hauteur2 t))
            (-1) fils;;


hauteur t;;
hauteur2 t;;

(*
---------------------------------------------------------
Q3 — Présence d’un élément dans l’arbre
---------------------------------------------------------
Recherche récursive dans les sous-arbres.
*)

let rec contient a x =
  match a with
  | Vide -> false
  | Noeud (v, fils) ->
      v = x || List.exists (fun t -> contient t x) fils;;
      


(*
---------------------------------------------------------
Q4 — Nombre d’occurrences d’un élément
---------------------------------------------------------
On ne suppose pas les étiquettes distinctes.
*)


let rec nb_occurrences a x =
	let  somme_liste l  = 
		List.fold_left (fun acc x  -> acc + x) 0 l 
	in 
  match a with
  | Vide -> 0
  | Noeud (v, fils)  when v = x ->
      1 +  somme_liste (List.map  (fun tree -> nb_occurrences tree x) fils)
  | Noeud (v, fils)  ->
      0 +  somme_liste (List.map  (fun tree -> nb_occurrences tree x) fils);;








c
let rec nb_occurrences2 a x =
  match a with
  | Vide -> 0
  | Noeud (v, fils) ->
      (if v = x then 1 else 0)
      + List.fold_left (fun acc t -> acc + nb_occurrences2 t x) 0 fils;;
      
      


(*
---------------------------------------------------------
Q5 — Chemin vers un élément
---------------------------------------------------------
On renvoie une option :
- None si absent
- Some [v0; ...; vk] sinon
*)



let rec chemin a x =
  match a with
  | Vide -> None
  | Noeud (v, fils) when v = x -> Some [v]
  | Noeud (v, []) -> None
  | Noeud (v, tree::q) -> 
  				match chemin (Noeud (v,q)) x  with 
  				Some path ->  Some path
  				|None ->  (match chemin tree x with 
  				          None -> None
  				          |Some path2 -> Some  (v:: path2));;
  				          
  				  	


let rec chemin2 a x =
  match a with
  | Vide -> None
  | Noeud (v, fils) ->
      if v = x then Some [v]
      else
        let rec chercher = function
          | [] -> None
          | t :: q ->
              match chemin2 t x with
              | None -> chercher q
              | Some l -> Some (v :: l)
        in
        chercher fils;;
        
chemin t 6;;
chemin2 t 6;;


(*
---------------------------------------------------------
Q5b — Tous les chemins vers une étiquette donnée
---------------------------------------------------------
On collecte tous les chemins possibles.
*)

let rec chemins a x =
  match a with
  | Vide -> []
  | Noeud (v, []) when v = x -> [[x]]
  |Noeud (v, []) -> []
  | Noeud (v, fils)  ->
      let sous_chemins =
        List.concat (List.map (fun t -> chemins t x) fils)
      in
      let sous_chemins_longs = List.map (fun l -> v :: l) sous_chemins in 
      if v = x then [x]::sous_chemins_longs else sous_chemins_longs;;
      
chemins t 6;;



 let tbis =
  Noeud (1, [
    Noeud (2, []);
    Noeud (3, [
      Noeud (5, []);
      Noeud (6, [])
    ]);
    Noeud (6, [])
  ]);;


chemins tbis 6;;

      

(*
---------------------------------------------------------
Q6 — Liste des feuilles
---------------------------------------------------------
Une feuille est un nœud sans fils.
*)

let rec feuilles a =
  match a with
  | Vide -> []
  | Noeud (v, []) -> [v]
  | Noeud (_, fils) ->
      List.concat (List.map feuilles fils);;


(*
---------------------------------------------------------
Q7a — Parcours préfixe
---------------------------------------------------------
Racine puis sous-arbres de gauche à droite.
*)

let rec parcours_prefixe a =
  match a with
  | Vide -> []
  | Noeud (v, fils) ->
      v :: List.concat (List.map parcours_prefixe fils);;


(*
---------------------------------------------------------
Q9 — Parcours postfixe
---------------------------------------------------------
Sous-arbres puis racine.
*)

let rec parcours_postfixe a =
  match a with
  | Vide -> []
  | Noeud (v, fils) ->
      List.concat (List.map parcours_postfixe fils) @ [v];;


(*
---------------------------------------------------------
Q8 — Étiquettes à un niveau donné
---------------------------------------------------------
Profondeur de la racine = 0.
*)

let rec etiquettes_niveau a k =
  match a with
  | Vide -> []
  | Noeud (v, fils) ->
      if k = 0 then [v]
      else
        List.concat
          (List.map (fun t -> etiquettes_niveau t (k - 1)) fils);;


(*
---------------------------------------------------------
Q9 — Test d’uniformité
---------------------------------------------------------
Un arbre est uniforme si tous les nœuds internes
ont le même nombre de fils.
*)

let rec est_uniforme a =
  match a with
  | Vide -> true
  | Noeud (_, []) -> true
  | Noeud (_, fils) ->
      let d = List.length fils in
      List.for_all
        (fun t ->
           match t with
           | Vide -> true
           | Noeud (_, f) ->
               List.length f = d && est_uniforme t)
        fils;;


(*
---------------------------------------------------------
Q10 — Degré maximal
---------------------------------------------------------
Le degré d’un nœud est son nombre de fils.
*)

let rec degre_max a =
  match a with
  | Vide -> 0
  | Noeud (_, fils) ->
      max (List.length fils)
          (List.fold_left
             (fun acc t -> max acc (degre_max t))
             0 fils);;


(*
---------------------------------------------------------
Q11 — Sous-arbre enraciné en une valeur
---------------------------------------------------------
On renvoie le premier sous-arbre trouvé.
*)

let rec sous_arbre a x =
  match a with
  | Vide -> None
  | Noeud (v, fils) ->
      if v = x then Some a
      else
        let rec chercher = function
          | [] -> None
          | t :: q ->
              match sous_arbre t x with
              | None -> chercher q
              | res -> res
        in
        chercher fils;;


(*
---------------------------------------------------------
Q14 — Plus petit ancêtre commun (PPAC)
---------------------------------------------------------
Principe :


On parcourt l’arbre une seule fois, et pour chaque nœud on détermine :

si le sous-arbre contient x,
s’il contient y,
et si le PPAC a déjà été trouvé plus bas.
*)



let ppac_lineaire a x y =
  let rec aux a =
    match a with
    | Vide -> (false, false, None)

    | Noeud (v, fils) ->

        (* Analyse récursive des sous-arbres *)
        let infos = List.map aux fils in

        (* Si un PPAC est déjà trouvé plus bas, on le propage *)
        let ppac_descendant =
          List.find_map (fun (_, _, p) -> p) infos
          (* On triche un peu en utilisant, List.find_map : 
          find_map f l applies f to the elements of l in order,
           and returns the first result of the form Some v, or None if none exist.*)
           
        in
        match ppac_descendant with
        | Some p -> (true, true, Some p)

        | None ->
            let contient_x_fils =
              List.exists (fun (cx, _, _) -> cx) infos
            in
            let contient_y_fils =
              List.exists (fun (_, cy, _) -> cy) infos
            in

            let contient_x = (v = x) || contient_x_fils in
            let contient_y = (v = y) || contient_y_fils in

            (* Si x et y sont trouvés ici, c’est le PPAC *)
            if contient_x && contient_y then
              (true, true, Some v)
            else
              (contient_x, contient_y, None)
  in
  let (_, _, res) = aux a in
  res;;
``
(* ========================= FIN ========================= *)