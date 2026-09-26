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
