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

(** Lignes d'horizon *)

type immeuble = int * int * int
type ville = immeuble list
type horizon = (int * int) list

let immeuble_to_hor (im : immeuble) : horizon =
  let deb, haut, fin = im in
  [ (deb, haut) ]

let compare (im1 : immeuble) (im2 : immeuble) : int =
  match (im1, im2) with
  | (g1, h1, d1), (g2, h2, d2) ->
      if g1 > g2 then 1 else if g1 < g2 then -1 else 0

let skyline (v : ville) : horizon =
  let n = List.length v in
  let vnew = List.sort compare v in
  let rec aux (current : immeuble) (i : int) (v : ville) (hor : horizon) :
      horizon =
    if i = n then hor
    else
      match v with
      | [] -> hor
      | x :: xs ->
          let g, h, d = x in
          let gc, hc, dc = current in
          if hc < h then aux (g, h, d) (i + 1) xs (immeuble_to_hor x @ hor)
          else if dc = i then aux x (i + 1) xs hor
          else if current = [] then
          else aux current (i + 1) xs hor
  in
  let ans = aux (0, 0, 0) 0 vnew [] in
  List.rev ans

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
