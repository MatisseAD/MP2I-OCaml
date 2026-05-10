(*
=========================================================
CORRIGÉ – EXO II : TAS BINOMIAUX (OCAML)
=========================================================

On utilise exclusivement des structures immuables
et de la récursion (style fonctionnel).
*)

(* ===================================================== *)
(* II.1 — Arbres binomiaux                               *)
(* ===================================================== *)

type arbre =
  | Vide
  | Noeud of int * arbre list;;

(* Q1 *)
let vide a =
  match a with
  | Vide -> true
  | _ -> false;;

let racine a =
  match a with
  | Vide -> failwith "Arbre vide"
  | Noeud (v, _) -> v;;

let fils a =
  match a with
  | Vide -> []
  | Noeud (_, l) -> l;;


(* ===================================================== *)
(* Q2 — Fusion de deux arbres binomiaux                  *)
(* ===================================================== *)


(* Attention, la fonction suivante ne marche que pour des arbres de même taille*)
let fusion_arbre a1 a2 =
  match a1, a2 with
  | Noeud (v1, l1), Noeud (v2, l2) ->
      if v1 <= v2 then
        Noeud (v1, a2 ::l1)
      else
        Noeud (v2, a1::l2)
  | _ -> failwith "Fusion impossible";;


(*
Q3a (théorique) :
Par récurrence, on montre que la racine d’un arbre binomial
d’ordre k possède exactement k fils.
 *)


(* ===================================================== *)
(* Q3b — Ordre d’un arbre binomial                       *)
(* ===================================================== *)

let ordre a =
  match a with
  | Vide -> failwith "Arbre vide,  ou bien -infini"
  | Noeud (_, l) -> List.length l;;


(*
Q4 (théorique) :
Un arbre binomial d’ordre k possède exactement 2^k nœuds.
Preuve par récurrence immédiate à partir de la définition, 
on utilise essentiellement 2^(k-1) + 2^(k-1)=2^k *)


(* ===================================================== *)
(* Q5 — Test de validité d’un arbre binomial            *)
(* ===================================================== *)


(* La fonction suivante ne teste que la structure*)




	    
	    
let rec est_arbre_ordre a k = 
	match a with 
	Vide -> false 
	|Noeud (v, []) -> k = 0 
	|Noeud (v, t::q) -> 
	    (est_arbre_ordre (Noeud (v, q)) (k-1) )
	     && (est_arbre_ordre t (k-1))&&
	      (v <= racine t);;
	    
let rec est_arbre_binomial a  =
	match a with 
	Vide -> true
	| Noeud (v, fils) -> est_arbre_ordre a (List.length fils);;

	
	    
(*exemples*)
est_arbre_binomial b3;;
	    

let b3 =
  Noeud (1, [
    Noeud (2, [
      Noeud (3, [
        Noeud (4, [])
      ]);
      Noeud (5, [])
    ]);
    Noeud (6, [
      Noeud (7, [])
    ]);
    Noeud (8, [])
  ]);;



(* ===================================================== *)
(* II.2 — Tas binomiaux                                  *)
(* ===================================================== *)

type tas = arbre list;;



(* ===================================================== *)
(* Q7 - Signature                                        *)
(* ===================================================== *)



(* La signature est la décomposition en base 2 du nombre
de noeuds*)

(* ===================================================== *)
(* Q8 — Minimum d’un tas                                *)
(* ===================================================== *)



(* Exemples: *)


let b0 =
  Noeud (4, []);;

let b2 =
  Noeud (2, [
    Noeud (3, [
      Noeud (4, [])
    ]);
    Noeud (5, [])
  ]);;
  

let b3 =
  Noeud (1, [
    Noeud (2, [
      Noeud (3, [
        Noeud (4, [])
      ]);
      Noeud (5, [])
    ]);
    Noeud (6, [
      Noeud (7, [])
    ]);
    Noeud (8, [])
  ]);;
  
  
 let extas : tas =
  [
    b0;    (* ordre 0 *)
    Vide;  (* ordre 1 *)
    b2;    (* ordre 2 *)
    b3     (* ordre 3 *)
  ];;

(* Le minimum d'un noeud c'est le minimum des racines des arbres*)

let mini_liste l = 
	List.fold_left min max_int l;;
	

let minimum tas = 
	(List.map (fun arbre -> match arbre with 
	          Vide -> max_int | _ -> racine arbre) tas) |> mini_liste;;


minimum extas;;	          
	          


(*
Complexité :
On parcourt au plus log(|T|) arbres,
donc complexité O(log |T|).
 *)


(* ===================================================== *)
(* Q16 — Insertion dans un tas binomial                  *)
(* ===================================================== *)


(* Rq: l'insertion ressemble beaucoup au calcul en base 2*)
  
  
  
 let insertion p t =
  let rec ins a  t =
  (* ajoute l'arbre a d'ordre i dans t qui 
  correspond à la liste des arbres d'ordre >= i 
  d'un tas binomial*)
      match t with
    | [] -> [a]
    | Vide :: q -> a :: q
    | ai :: q ->
        let a' = fusion_arbre a ai in
        Vide :: (ins a'  q)
  in
  let a0 = Noeud (p, []) in
  ins a0  t;;


insertion 5 (insertion 1 (insertion 3 [Vide; Vide; Vide; b3]));;
  
  

(*
Q10 (qualitative) :
- Le nombre de fusions est borné par le nombre d’ordres et chaque fusion est en O(1)
- soit O(log |T|)
 *)







(* ===================================================== *)
(* II.3 — Suppression du minimum                         *)
(* ===================================================== *)


(* Q13: il faut: 
1. chercher min parmi les racines
2. retirer l’arbre correspondant
3. prendre ses sous-arbres  dans le bon 
4. fusionner avec le reste du tas cela ressemble à une addition en base 2*)



(* Extraction de l’arbre contenant le minimum *)

let rec extraire_arbre_min t =
  match t with
  | [] -> failwith "Tas vide"
  | [a] -> a, []
  | Vide :: q ->
      let a_min, reste = extraire_arbre_min q in
      a_min, Vide :: reste
  | a :: q ->
      let a_min, reste = extraire_arbre_min q in
      if racine a <= racine a_min then
        a, Vide :: q
      else
        a_min, a :: reste;;



(* Fusion de deux tas binomiaux: rq, les fils d'un arbre bonomial forment un tas binomial*)


let fusion_tas t1 t2 =
  let rec aux retenue t1 t2 =
    match retenue, t1, t2 with
    | None, [], [] -> []
    | Some r, [], [] -> [r]

    | r, a1 :: q1, [] ->
        aux r (a1 :: []) []
    | r, [], a2 :: q2 ->
        aux r [] (a2 :: [])

    | None, Vide :: q1, Vide :: q2 ->
        Vide :: aux None q1 q2

    | None, a :: q1, Vide :: q2
    | None, Vide :: q1, a :: q2 ->
        a :: aux None q1 q2

    | None, a1 :: q1, a2 :: q2 ->
        Vide :: aux (Some (fusion_arbre a1 a2)) q1 q2

    | Some r, Vide :: q1, Vide :: q2 ->
        r :: aux None q1 q2

    | Some r, a :: q1, Vide :: q2
    | Some r, Vide :: q1, a :: q2 ->
        Vide :: aux (Some (fusion_arbre r a)) q1 q2

    | Some r, a1 :: q1, a2 :: q2 ->
        let r1 = fusion_arbre a1 a2 in
        Vide :: aux (Some (fusion_arbre r r1)) q1 q2
  in
  aux None t1 t2;;

(* ===================================================== *)
(* Q21 — Suppression du minimum                          *)
(* ===================================================== *)




let extraire_min tas =
  let arbre_min, tas_sans_min = extraire_arbre_min tas in
  match arbre_min with
  | Vide -> failwith "Impossible"
  | Noeud (v, fils) ->
      (* Les fils sont d’ordres décroissants :
         on les inverse pour faire un tas valide *)
      let tas_fils = List.rev fils in
      let nouveau_tas =
        fusion_tas tas_sans_min tas_fils
      in
      (v, nouveau_tas);;

(*
Q22 (qualitative) :
- extraction du minimum : O(log |T|)
- réinsertion de log |T| arbres
→ complexité totale : O(log |T|)
 *)

(* ======================= FIN ========================= *)
``