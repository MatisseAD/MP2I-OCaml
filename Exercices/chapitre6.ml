let debarquement =
  [
    "Les";
    "sanglots";
    "longs";
    "des";
    "violons";
    "de";
    "l'automne";
    "blessent";
    "mon";
    "coeur";
    "d'une";
    "langueur";
    "monotone.";
    "Tout";
    "suffocant";
    "et";
    "bleme";
    "quand";
    "sonne";
    "l'heure,";
    "je";
    "me";
    "souviens";
    "des";
    "jours";
    "anciens";
    "et";
    "je";
    "pleure";
    "et";
    "je";
    "m'en";
    "vais";
    "au";
    "vent";
    "mauvais";
    "qui";
    "m'emporte";
    "deca";
    "dela";
    "pareil";
    "a";
    "la";
    "feuille";
    "morte.";
  ]

let cpt_caractere (s : string) = String.length s

let rec create_space_string (l : int) =
  if l = 0 then "" else " " ^ create_space_string (l - 1)

let rec cpt_liste_caractere (ls : string list) =
  match ls with [] -> 0 | x :: xs -> cpt_caractere x + cpt_liste_caractere xs

let ligne_to_affichable (lg : int) (ligne : string list) : string =
  let rec aux (cpt : int) (ligne : string list) (premier : bool) :
      string list * string list =
    match ligne with
    | [] -> ([], [])
    | mot :: mots ->
        let taille = cpt_caractere mot + if premier then 0 else 1 in
        if taille > cpt then ([], ligne)
        else
          let an1, an2 = aux (cpt - taille) mots false in
          (mot :: an1, an2)
  in
  let x, y = aux lg ligne true in
  let xbis = ref x in
  let ybis = ref y in
  let ans = ref "" in
  while !xbis <> [] do
    if !ybis <> [] && List.length !xbis > 1 then begin
      let n = List.length !xbis in
      let total_chars = cpt_liste_caractere !xbis in
      let total_spaces = lg - total_chars in
      let gaps = n - 1 in
      let base = total_spaces / gaps in
      let extra = total_spaces mod gaps in
      let rec aux2 words i =
        match words with
        | [] -> ""
        | [ mot ] -> mot
        | mot :: mots ->
            let espaces = base + if i < extra then 1 else 0 in
            mot ^ String.make espaces ' ' ^ aux2 mots (i + 1)
      in
      ans := !ans ^ aux2 !xbis 0
    end
    else ans := !ans ^ String.concat " " !xbis;
    let temp = aux lg !ybis true in
    xbis := fst temp;
    ybis := snd temp
  done;
  !ans

let ligne_to_affichable_space_counter (lg : int) (ligne : string list) : int =
  let rec aux (cpt : int) (ligne : string list) (premier : bool) :
      string list * string list =
    match ligne with
    | [] -> ([], [])
    | mot :: mots ->
        let taille = cpt_caractere mot + if premier then 0 else 1 in
        if taille > cpt then ([], ligne)
        else
          let an1, an2 = aux (cpt - taille) mots false in
          (mot :: an1, an2)
  in
  let x, y = aux lg ligne true in
  let xbis = ref x in
  let ybis = ref y in
  let ans = ref "" in
  let cpt = ref 0 in
  while !xbis <> [] do
    if !ybis <> [] && List.length !xbis > 1 then begin
      let n = List.length !xbis in
      let total_chars = cpt_liste_caractere !xbis in
      let total_spaces = lg - total_chars in
      let gaps = n - 1 in
      let base = total_spaces / gaps in
      let extra = total_spaces mod gaps in
      let rec aux2 words i =
        match words with
        | [] -> ""
        | [ mot ] -> mot
        | mot :: mots ->
            let espaces = base + if i < extra then 1 else 0 in
            cpt := !cpt + espaces;
            mot ^ String.make espaces ' ' ^ aux2 mots (i + 1)
      in
      ans := !ans ^ aux2 !xbis 0
    end
    else begin
      cpt := !cpt + 1;
      ans := !ans ^ String.concat " " !xbis
    end;
    let temp = aux lg !ybis true in
    xbis := fst temp;
    ybis := snd temp
  done;
  !cpt

let rec affiche (liste_string : string list) : unit =
  let rec aux liste_string =
    match liste_string with mot :: mots -> mot ^ " " ^ aux mots | [] -> ""
  in
  let s = aux liste_string in
  print_string s

let essai1 =
  [
    "Je";
    "m'";
    "appelle";
    "Léandre";
    "Garcia";
    "et";
    "j'";
    "aime";
    "le";
    "chocolat";
  ]

let rec glouton (lg : int) (liste_mots : string list) : string list list =
  if liste_mots = [] then []
  else
    let rec aux lg lm =
      match (lg, lm) with
      | _, [] -> ([], [])
      | x, mot :: mots ->
          if cpt_caractere mot > x then ([], mot :: mots)
          else
            let f, s = aux (lg - cpt_caractere mot) mots in
            (mot :: f, s)
    in
    let f, s = aux lg liste_mots in
    f :: glouton lg s

let rec affiche l =
  match l with
  | [] -> ()
  | mot :: mots ->
      print_string mot;
      print_newline ();
      affiche mots

let affichage_glouton (lg : int) (liste_mot : string list) : unit =
  affiche
    (List.map
       (fun ligne -> ligne_to_affichable lg ligne)
       (glouton lg liste_mot))

let rec fabric_tab_lg (liste_mots : string list) =
  match liste_mots with
  | mot :: mots -> String.length mot :: fabric_tab_lg mots
  | [] -> []

let cout (i : int) (j : int) (tab_lg : int array) (lg : int) =
  let rec aux i cpt =
    if i > j then 0
    else if tab_lg.(i) + cpt > j then
      let space = lg - cpt in
      aux 0 cpt + (space * space)
    else aux (i + 1) (cpt + tab_lg.(i))
  in
  aux i 0

(** Formule de récurrence d(i) *)

(** Exercice 6.5.2 *)

(** Exercice 6.5.5 *)

let min3 a b c = min (min a b) c
let f c1 c2 = if c1 = c2 then 0 else if c1 = ' ' || c2 = ' ' then 1 else 1

let bestscore u v f =
  let n = String.length u in
  let m = String.length v in
  let s = Array.make_matrix n m 0 in
  s.(0).(0) <- 0;
  for j = 1 to m - 1 do
    s.(0).(j) <- s.(0).(j - 1) + 1
  done;
  for i = 1 to n - 1 do
    s.(0).(i) <- 1 + s.(0).(i - 1)
  done;
  for i = 1 to n - 1 do
    for j = 1 to m - 1 do
      let a = s.(i - 1).(j - 1) + f u.[i - 1] v.[j - 1]
      and b = s.(i).(j - 1) + f ' ' v.[j - 1]
      and c = s.(i - 1).(j) + f u.[i - 1] ' ' in
      s.(i).(j) <- min3 a b c
    done
  done;
  s.(n - 1).(m - 1)

let meilleur_PD u v =
  let n = String.length u in
  let m = String.length v in
  let s = Array.make_matrix n m 0 in
  let so = Array.make_matrix n m 0 in
  s.(0).(0) <- 0;
  for j = 1 to m - 1 do
    s.(0).(j) <- s.(0).(j - 1) + 1
  done;
  for i = 1 to n - 1 do
    s.(0).(i) <- 1 + s.(0).(i - 1)
  done;
  for i = 1 to n - 1 do
    for j = 1 to m - 1 do
      let a = s.(i - 1).(j - 1) + f u.[i - 1] v.[j - 1]
      and b = s.(i).(j - 1) + f ' ' v.[j - 1]
      and c = s.(i - 1).(j) + f u.[i - 1] ' ' in
      s.(i).(j) <- min3 a b c
    done
  done;
  s.(n - 1).(m - 1)

type immeuble = int * int * int
(** Lignes d'horizon *)

type ville = immeuble list
type horizon = (int * int) list

let rec nettoyer (hor : horizon) : horizon =
  match hor with
  | (x1, y1) :: (x2, y2) :: suite when x1 = x2 ->
      nettoyer ((x1, max y1 y2) :: suite)
  | (x1, y1) :: (x2, y2) :: suite when y1 = y2 -> nettoyer ((x1, y1) :: suite)
  | point :: suite -> point :: nettoyer suite
  | [] -> []

let rec fusion_horizons (hor1 : horizon) (hor2 : horizon) (h1 : int) (h2 : int)
    : horizon =
  match (hor1, hor2) with
  | [], _ -> hor2
  | _, [] -> hor1
  | (x1, y1) :: suite1, (x2, y2) :: suite2 ->
      if x1 < x2 then
        let h_max = max y1 h2 in
        (x1, h_max) :: fusion_horizons suite1 hor2 y1 h2
      else if x1 > x2 then
        let h_max = max h1 y2 in
        (x2, h_max) :: fusion_horizons hor1 suite2 h1 y2
      else (* x1 = x2 *)
        let h_max = max y1 y2 in
        (x1, h_max) :: fusion_horizons suite1 suite2 y1 y2

let rec skyline (v : ville) : horizon =
  match v with
  | [] -> []
  | [ (g, h, d) ] -> [ (g, h); (d, 0) ]
  | _ ->
      let n = List.length v in
      let rec separer lst i =
        if i = 0 then ([], lst)
        else
          match lst with
          | [] -> ([], [])
          | x :: xs ->
              let gauche, droite = separer xs (i - 1) in
              (x :: gauche, droite)
      in
      let moitie1, moitie2 = separer v (n / 2) in

      let h1 = skyline moitie1 in
      let h2 = skyline moitie2 in

      nettoyer (fusion_horizons h1 h2 0 0)

let cas_vide : ville = []
let cas_unique : ville = [ (2, 10, 9) ]
let cas_disjoints : ville = [ (1, 5, 3); (5, 8, 8) ]
let cas_adjacents_A : ville = [ (1, 4, 3); (3, 7, 6) ]
let cas_adjacents_B : ville = [ (1, 7, 3); (3, 4, 6) ]
let cas_fusion_hauteur : ville = [ (1, 5, 3); (3, 5, 6) ]
let cas_cache_total : ville = [ (1, 10, 8); (3, 5, 6) ]
let cas_escalier : ville = [ (1, 4, 5); (3, 8, 7) ]
let cas_trou_milieu : ville = [ (1, 5, 10); (4, 12, 7) ]

let crash_test : ville =
  [ (2, 10, 9); (3, 5, 7); (8, 12, 12); (10, 12, 15); (19, 8, 22) ]
