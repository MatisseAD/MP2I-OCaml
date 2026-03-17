(** Exercice 4.1 **)

let affiche_pile (pile : int Stack.t) : unit =
  while not (Stack.is_empty pile) do
    let a = Stack.pop pile in
    print_int a
  done

(** Après éxécution de cette fonction, la pile sera malehuresement vide ... **)

let rec liste_to_pile (liste : 'a list) (pile : 'a Stack.t) : unit =
  match liste with
  | x :: xs ->
      Stack.push x pile;
      liste_to_pile xs pile
  | [] -> ()

let rec pile_to_liste (pile : 'a Stack.t) : 'b list =
  if not (Stack.is_empty pile) then
    let x = Stack.pop pile in
    x :: pile_to_liste pile
  else []

let iter_pile f p =
  let p_list = pile_to_liste p in
  List.iter f p_list

let show_elements p =
  let f x = print_int x in
  iter_pile f p

let sum_pile p =
  let cpt = ref 0 in
  let f x = cpt := x + !cpt in
  iter_pile f p

(** Exercice 4.2 **)

(** Q1

    CF chapitre4.md *)

let engendrable (t : int list) : bool =
  let stack = Stack.create () in
  Stack.push 1 stack;
  let rec aux (t : int list) (value_str : string) (cpt : int) =
    match t with
    | [] -> value_str
    | x :: xs ->
        let top = Stack.top stack in
        if top < x then begin
          Stack.push (cpt + 1) stack;
          aux (x :: xs) (value_str ^ "E") (cpt + 1)
        end
        else if top > x then begin
          aux xs (value_str ^ "F") cpt
        end
        else begin
          let _ = Stack.pop stack in
          aux xs (value_str ^ "D") cpt
        end
  in
  let ans = aux t "E" 1 in
  if String.contains ans 'F' then false
  else (
    print_string ans;
    true)

(** Exercice 4.4 : TasMin*)

type 'a tasmin = { mutable taille : int; arbre : 'a array }

(** 1)a) *)

let i_pere (k : int) : int = k / 2
let i_fg (k : int) : int = 2 * k
let i_fd (k : int) : int = (2 * k) + 1
let etiq (k : int) (t : 'a tasmin) : 'a = t.arbre.(k)

let fg (k : int) (t : 'a tasmin) : int =
  let indice = i_fg k in
  if indice > t.taille then -1 else t.arbre.(indice)

let fd (k : int) (t : 'a tasmin) : int =
  let indice = i_fd k in
  if indice > t.taille then -1 else t.arbre.(indice)

let echange (k : int) (kp : int) (t : 'a tasmin) : unit =
  assert (kp <= t.taille);
  let tampon = t.arbre.(k) in
  t.arbre.(k) <- t.arbre.(kp);
  t.arbre.(kp) <- tampon

let is_good_father (k : int) (t : 'a tasmin) : bool =
  if fg k t <> -1 && fd k t <> -1 then begin
    if t.arbre.(k) <= fg k t && t.arbre.(k) < fd k t then true else false
  end
  else if fg k t = -1 && fd k t <> -1 then begin
    if t.arbre.(k) < fd k t then true else false
  end
  else if fd k t = -1 then if fg k t > t.arbre.(k) then true else false
  else true

let remonte (k : int) (t : 'a tasmin) : unit =
  let daddy = ref (i_pere k) in
  let i = ref k in
  while etiq !daddy t > etiq !i t && !i <> 0 do
    echange !i !daddy t;
    i := !daddy;
    daddy := i_pere !i
  done

let descend (k : int) (t : 'a tasmin) : unit =
  let i = ref k in
  while not (is_good_father !i t) do
    if t.arbre.(fg !i t) < t.arbre.(fd !i t) then begin
      echange !i (i_fg !i) t;
      i := i_fg !i
    end
    else begin
      echange !i (i_fd !i) t;
      i := i_fd !i
    end
  done

let insere (x : 'a) (t : 'a tasmin) : unit =
  t.taille <- t.taille + 1;
  t.arbre.(t.taille) <- x;
  remonte t.taille t

let pop_min (t : 'a tasmin) : 'a =
  let ans = t.arbre.(1) in
  for i = 2 to t.taille do
    t.arbre.(i - 1) <- t.arbre.(i)
  done;
  ans

let rec liste_to_tas (l : 'a list) (t : 'a tasmin) : 'a tasmin =
  match l with
  | [] -> t
  | x :: xs ->
      insere x t;
      liste_to_tas xs t

let tas_to_list (t : 'a tasmin) : 'a list =
  let rec aux l =
    if t.taille = 0 then l
    else
      let x = pop_min t in
      aux (x :: l)
  in
  aux []

type 'a fileprio = (int * 'a) tasmin

(** Exercice 4.3 *)

type graphe = int list array
