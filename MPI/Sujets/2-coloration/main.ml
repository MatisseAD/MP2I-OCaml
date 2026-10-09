type graphe = int * int list array
type color = BLANC | NOIR | VIDE

let color_complementaire (c : color) =
  match c with
  | BLANC -> NOIR
  | NOIR -> BLANC
  | VIDE -> failwith "VIDE n'a pas de couleur complémentaire"

let dfs (g : graphe) : color array =
  let n, a = g in
  let coloration = Array.make n VIDE in
  let rec dfs_rec (i : int) (couleur : color) : unit =
    let rec aux (l : int list) =
      if coloration.(i) = VIDE then coloration.(i) <- couleur;
      match l with
      | x :: xs ->
          if coloration.(x) = VIDE then dfs_rec x (color_complementaire couleur)
          else aux xs
      | [] -> ()
    in
    aux a.(i);
  in
  for i = 0 to n - 1 do
    if coloration.(i) = VIDE 
      then
        (** On regarde si un des sommets limitrophes a été coloré*)
        let couleur_a_utiliser = 
          let rec sommets_relies_deja_colore aretes =
          match aretes with
          | [] -> NOIR
          | w :: ws -> if coloration.(w) <> VIDE then (color_complementaire coloration.(w)) else sommets_relies_deja_colore ws
          in
          sommets_relies_deja_colore a.(i)
        in
        dfs_rec i couleur_a_utiliser
  done;
  coloration

let color_verif (g : graphe) (coloration : color array) : bool =
  let ans = ref true in
  let n, a = g in
  for i = 0 to n - 1 do
    let rec aux (a : int list) =
      match a with
      | [] -> ()
      | w :: ws -> if coloration.(w) = coloration.(i) 
        then 
          ans := false
        else aux ws
    in
    aux a.(i)
  done;
  !ans

let est_2_coloriable (g : graphe) =
  let coloration = dfs g in
  let n = Array.length coloration in
  for i = 0 to n - 1 do
    Printf.printf "Sommet %d : %s\n" i
      (match coloration.(i) with
      | BLANC -> "BLANC"
      | NOIR -> "NOIR"
      | VIDE -> "VIDE")
  done;
  color_verif g coloration

type tri_color = BLEU | BLANC | ROUGE | VIDE

(** Graphe de test qui est 3-coloriable *)
let g_test_3_color = (4, [| [ 1; 2 ]; [ 0; 3 ]; [ 0; 3 ]; [ 1; 2 ] |])

let pow_three (n : int) : int =
  let ans = ref 1 in
  for i = 0 to n - 1 do
    ans := !ans * 3
  done;
  !ans

let generate_random_coloration (t : int array) (cpt : int) =
  let n = Array.length t in
  let cpt_aux = ref (cpt + 1) in
  if !cpt_aux >= pow_three n then None
  else
    let i = ref 0 in
    let ans = Array.make n 0 in
    while !cpt_aux <> 0 do
      let r = !cpt_aux mod 3 in
      ans.(!i) <- r;
      cpt_aux := !cpt_aux / 3;
      incr i
    done;
    if !i >= n then None else Some ans

let transform_to_coloration (t : int array) =
  let n = Array.length t in
  let t' = Array.make n VIDE in
  for i = 0 to n - 1 do
    match t.(i) with
    | 0 -> t'.(i) <- BLEU
    | 1 -> t'.(i) <- BLANC
    | 2 -> t'.(i) <- ROUGE
    | _ -> failwith "Impossible"
  done;
  t'

let color_verif_tri (g : graphe) (t : tri_color array) =
  let n, a = g in
  let ans = ref true in
  for i = 0 to n - 1 do
    let rec aux l =
      match l with
      | [] -> ()
      | w :: ws -> if t.(i) = t.(w) then ans := false else aux ws
    in
    aux a.(i)
  done;
  !ans

let est_3_coloriable (g : graphe) =
  let n, a = g in
  let ans = ref true in
  let t = ref (Array.make n 0) in
  let res = ref None in
  let cpt = ref 0 in
  while !ans do
    let t' = generate_random_coloration !t !cpt in
    match t' with
    | None -> ans := false
    | Some t'' ->
        let t_final = transform_to_coloration t'' in
        cpt := !cpt + 1;

        if color_verif_tri g t_final then (
          res := Some t_final;
          ans := false)
        else ans := true
  done;
  !res


(** Réduction 3-sat vers 3-color *)


type clause = int * int * int

type formule = clause list

type sat_graphe = int list array


let nb_litt (f : formule) : int =
  let rec aux (f : formule) (cpt : int) : int =
    match f with
    | [] -> cpt
    | (l1,l2,l3) :: xs ->
      aux xs (max cpt (max (max (abs l1) (abs l2)) (abs l3)))
    in aux f 0

let nb_clause (f : formule) = List.length f

let est3_coloriable_sat (g : sat_graphe) =
  let n = Array.length g in
  let graphe : graphe = (n, g) in
  est_3_coloriable graphe

let sat_vers_coloriable (f : formule) =

  let nblitt = nb_litt f in
  let nbclause = nb_clause f in
  let nbsommet = 6*nbclause + 2*nblitt + 3 in
let graphe : sat_graphe = Array.init nbsommet (fun _ -> []) in  
let get_indice_litt (l : int) : int =
    if l >= 0 then 2*(l-1) else 2*(abs l - 1) + 1
  in
  (**
  On considère que les 2*(nbclause-1) première case comme les cases réservées aux sommets qui représentent les littéraux et leur négation

  Exemple :
  Case 0 : x0
  Case 1 : ¬x0
  Case 2 : x1
  Case 3 : ¬x1
  ...
  *)

  (**
  On relie les arêtes xi et ¬xi entre elles
  *)

  let rec relier_x_et_non_x (g : sat_graphe) (cpt : int) =
    if cpt > 2 * (nblitt) - 1 then
      ()
    else if cpt mod 2 = 0 then (
      g.(cpt) <- (cpt + 1) :: g.(cpt);
      relier_x_et_non_x g (cpt + 1)
    ) else (
      g.(cpt) <- (cpt - 1) :: g.(cpt);
      relier_x_et_non_x g (cpt + 1)
    )
  in
  relier_x_et_non_x graphe 0;
      
  (**
  1. Le triangle de référence (La Palette)
  On commence par créer un triangle de base avec 3 sommets spéciaux reliés deux à deux : 
  Un sommet Vrai (True / T) -> graphe.(2*nblitt)
  Un sommet Faux (False / F) -> graphe.(2*nblitt+1)
  Un sommet Base (Neutre / N) -> graphe.(2*nblitt+2)
  Ces trois sommets doivent recevoir des couleurs distinctes dans toute 3-coloration valide. Ils servent de référence pour attribuer les valeurs de vérité.
  **)

  (** On relie les arêtes du triangle de référence *)
  graphe.(2*nblitt) <- (2*nblitt+1) :: (2*nblitt+2) :: graphe.(2*nblitt);
  graphe.(2*nblitt+1) <- (2*nblitt) :: (2*nblitt+2) :: graphe.(2*nblitt+1);
  graphe.(2*nblitt+2) <- (2*nblitt) :: (2*nblitt+1) :: graphe.(2*nblitt+2);

  (** On relie toutes les arrêtes au sommet Base *)
  let rec relier_aretes_to_sommet_base (g : sat_graphe) (cpt : int) =
    if cpt > 2*(nblitt) - 1 then ()
    else
      g.(cpt) <- 2*nblitt+2 :: g.(cpt);
      g.(2*nblitt+2) <- cpt :: g.(2*nblitt+2);
      relier_aretes_to_sommet_base g (cpt + 1)
  in 
  let _ = relier_aretes_to_sommet_base graphe 0 in

  (**
  
  3. "Gadget" pour les clauses
  Pour chaque clause C_j = (a v b v c) où a, b, c sont des littéraux, on construit un sous-graphe spécifique appelé gadget de clause.
Ce gadget possède trois sommets d'entrée correspondant aux littéraux a, b et c, et un sommet de sortie.
Le sommet de sortie du gadget est relié à la fois au sommet Faux et au sommet Base.
-> Propriété logique du gadget :
    Si les trois littéraux a, b, c sont colorés à la couleur Faux, il est impossible de 3-colorer le gadget de manière à ce que les contraintes soient respectées (cela force une contradiction avec le sommet Faux/Base).
    Dès qu'au moins un des littéraux a, b ou c est coloré Vrai, il existe toujours une 3-coloration valide pour ce sous-graphe.
  *)

  (**
  
  Représentation des gadgets :
  
  La clause c0 associé au gadget 0 pour le littéral l0 de c0 est à la case g.(nblitt+3)
  La clause c0 associé au gadget 0 pour le littéral l1 de c0 est à la case g.(nblitt+4)
  Le troisième sommet du gadget 0 de c0 est associé à la case g.(nblitt+5)
  La clause c0 associé au gadget 1 pour le littéral (l1,l2), (donc le troisième sommet du gadget 0) de c0 est à la case g.(nblitt+6)
  La clause c0 associé au gadget 1 pour le littéral l2 de c0 est à la case g.(nblitt+7)
  Le sommet "final" du gadget 1 de c0 est à la case g.(nblitt+8)
  *)
  let rec creer_gadget (f : formule) (g : sat_graphe) (cpt : int) : unit =
    
    match f with
    | [] -> ()
    | (i1,i2,i3) :: xs ->
      let l0 = get_indice_litt i1 in
      let l1 = get_indice_litt i2 in
      let l2 = get_indice_litt i3 in
      g.(2*nblitt+3+cpt) <- l0 :: g.(2*nblitt+3+cpt);
      g.(l0) <- (2*nblitt+3+cpt) :: g.(l0);
      g.(2*nblitt+4+cpt) <- l1 :: g.(2*nblitt+4+cpt);
      g.(l1) <- (2*nblitt+4+cpt) :: g.(l1);
      g.(2*nblitt+5+cpt) <- (2*nblitt+3+cpt) :: (2*nblitt+4+cpt) :: g.(2*nblitt+5+cpt);
      g.(2*nblitt+3+cpt) <- (2*nblitt+5+cpt) :: g.(2*nblitt+3+cpt);
      g.(2*nblitt+4+cpt) <- (2*nblitt+5+cpt) :: g.(2*nblitt+4+cpt);
      g.(2*nblitt+5+cpt) <- (2*nblitt+6+cpt) :: (2*nblitt+7+cpt) :: g.(2*nblitt+5+cpt);
      g.(2*nblitt+6+cpt) <- 2*nblitt+5+cpt :: g.(2*nblitt+6+cpt);
      g.(2*nblitt+7+cpt) <- l2 :: g.(2*nblitt+7+cpt);
      g.(l2) <- (2*nblitt+7+cpt) :: g.(l2);
      g.(2*nblitt+8+cpt) <- (2*nblitt+6+cpt) :: (2*nblitt+7+cpt) :: g.(2*nblitt+8+cpt);
      g.(2*nblitt+6+cpt) <- (2*nblitt+8+cpt) :: g.(2*nblitt+6+cpt);
      g.(2*nblitt+7+cpt) <- (2*nblitt+8+cpt) :: g.(2*nblitt+7+cpt);
      (** On relie le sommet de sortie du gadget au noeud base et faux*)
      g.(2*nblitt+8+cpt) <- (2*nblitt+1) :: (2*nblitt+2) :: g.(2*nblitt+8+cpt);
      g.(2*nblitt+1) <- (2*nblitt+8+cpt) :: g.(2*nblitt+1);
      g.(2*nblitt+2) <- 2*nblitt+8+cpt :: g.(2*nblitt+2);
      creer_gadget xs g (cpt+6);
  in creer_gadget f graphe 0;
  est3_coloriable_sat graphe;


