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

let degre g i =
  let l = g.(i) in
  List.length l

let degre_max g =
  let max = ref (degre g 0) in
  for i = 1 to Array.length g - 1 do
    if !max < degre g i then max := degre g i
  done;
  !max

let composante_connexe g s =
  let q = Queue.create () in
  let deja_vu = Array.make (Array.length g) false in
  let resultat = ref [] in
  let enfiler x =
    if not deja_vu.(x) then begin
      deja_vu.(x) <- true;
      Queue.push x q;
      resultat := x :: !resultat
    end
  in
  enfiler s;
  let rec aux () =
    if Queue.is_empty q then !resultat
    else
      let courant = Queue.pop q in
      List.iter enfiler g.(courant);
      aux ()
  in
  aux ()

let accessible g i j =
  let l = composante_connexe g i in
  let ans = ref false in
  let rec aux l =
    match l with
    | [] -> ()
    | x :: xs ->
        if x = j then begin
          ans := true;
          aux xs
        end
        else aux xs
  in
  aux l;
  !ans

(** On a écrit un parcourt en largeur, effectuons un parcours en profondeur *)

let profParcours g s =
  let dejaVu = Array.make (Array.length g) false in
  dejaVu.(s) <- true;
  let rec aux (l : int list) (ans : int list) =
    match l with
    | [] -> ans
    | x :: xs ->
        if not dejaVu.(x) then begin
          dejaVu.(x) <- true;
          aux (g.(x) @ xs) (x :: ans)
        end
        else aux xs ans
  in
  aux g.(s) [ s ]

let graphe_iter f g i =
  let composante = profParcours g i in
  List.iter f composante

let affiche g i =
  let f x =
    print_int x;
    print_newline ()
  in
  graphe_iter f g i

(** Exercice 4.5 *)

type sommet = int
type arete = int * sommet * sommet
type graphe = arete list
type compo = int array

let est_copain comp som1 som2 = comp.(som1) = comp.(som2)

let fusion comp som1 som2 =
  if not (est_copain comp som1 som2) then
    let n = Array.length comp in
    if comp.(som1) < comp.(som2) then begin
      let pivot = comp.(som2) in
      for i = 0 to n - 1 do
        if est_copain comp pivot i then comp.(i) <- comp.(som1)
      done
    end
    else begin
      let pivot = comp.(som1) in
      for i = 0 to n - 1 do
        if est_copain comp pivot i then comp.(i) <- comp.(som2)
      done
    end

let liste_copains (comp : compo) (som : sommet) : sommet list =
  let rec aux (i : int) (ans : sommet list) =
    match i with
    | 0 -> ans
    | _ ->
        if est_copain comp comp.(som) comp.(i) && som <> i then
          aux (i - 1) (comp.(i) :: ans)
        else aux (i - 1) ans
  in
  aux (Array.length comp - 1) []

(** Complexité de est compain : O(1) => Comparaison Complexité de fusion : O(n)
    => Parcours de l'array Coplexité de liste copains : O(n) => Parcours de
    l'array *)

(**let composantes (g : graphe) : int array = let t = Array.make (List.length g)
   0 in let rec aux arrete = match arrete with | [] -> t | x :: xs ->**)

(** Exercice 4.6 *)

type sommet = int
type unionfind = { pere : sommet array; hauteur : int array }

let chacun_pour_soi (n : int) : unionfind =
  let papa = Array.make n 0 in
  let size = Array.make n 0 in
  for i = 0 to n - 1 do
    papa.(i) <- i
  done;
  { pere = papa; hauteur = size }

let rec racine (s : sommet) (uf : unionfind) : sommet =
  let papa = uf.pere in
  if papa.(s) = s then s else racine papa.(s) uf

let est_relie (s : sommet) (sp : sommet) (uf : unionfind) : bool =
  let r1 = racine s uf in
  let r2 = racine sp uf in
  r1 = r2

let fusionne (s : sommet) (sp : sommet) (uf : unionfind) : unit =
  let papa = uf.pere in
  let h = uf.hauteur in
  if h.(s) > h.(sp) then (
    let oldr = racine sp uf in
    let newr = racine s uf in
    papa.(oldr) <- newr;
    h.(oldr) <- 0;
    h.(sp) <- h.(sp) + 1)
  else
    let oldr = racine s uf in
    let newr = racine sp uf in
    papa.(oldr) <- newr;
    h.(oldr) <- 0;
    h.(sp) <- h.(sp) + 1

let g1 = chacun_pour_soi 10;;

fusionne 0 1 g1;;
fusionne 1 2 g1;;
fusionne 2 3 g1;;
fusionne 3 4 g1;;
fusionne 5 6 g1;;
fusionne 7 8 g1;;
fusionne 5 7 g1;;
g1
