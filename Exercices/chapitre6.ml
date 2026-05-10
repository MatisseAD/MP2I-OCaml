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
      aux 0 cpt + space * space
    else aux (i + 1) (cpt + tab_lg.(i))
  in
  aux i 0

(** Formule de récurrence d(i) 



*)

let rec calcul_d_min (i : int) (tab_lg : int array) (lg : int) : int =
  if i = Array.length tab_lg then
     0
  else
    