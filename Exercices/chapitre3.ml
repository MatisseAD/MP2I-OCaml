(**Exercice 3.1 **)

let rec suite (n : int) =
  match n with
  | 0 -> [ 0 ]
  | p ->
      let rec filtre (l : int list) =
        match l with
        | [] -> []
        | x :: xs -> if x = 0 then 1 :: 0 :: filtre xs else 0 :: 1 :: filtre xs
      in
      filtre (suite (p - 1))

(** Exercice 3.2*)

let rec coupure liste n =
  match (n, liste) with
  | _, [] -> (liste, [])
  | 1, x :: xs -> ([ x ], xs)
  | p, x :: xs ->
      let a, b = coupure xs (p - 1) in
      (x :: a, b)

let shuffle l1 l2 =
  let rec aux l1 l2 turn =
    match (l1, l2) with
    | _, [] -> l1
    | [], _ -> l2
    | x :: xs, y :: ys ->
        if turn = 0 then x :: aux xs (y :: ys) 1 else y :: aux (x :: xs) ys 0
  in
  if List.length l1 > List.length l2 then aux l1 l2 0 else aux l1 l2 1

(** Exercice 3.3 *)

let estsousliste l1 l2 =
  if List.length l1 > List.length l2 then false
  else
    let rec aux l1 l2 =
      match (l1, l2) with
      | _, [] -> false
      | [], _ -> true
      | x :: xs, y :: ys -> if x = y then aux xs ys else aux (x :: xs) ys
    in
    aux l1 l2

(**Exercice 3.4*)

(**1. Code de Gray d'ordre 3

   Il faut donc représenter les entiers dans l'intervalle :

   I = [0;7]

   Binaire pur

   [0;0;0] = 0 [0;0;1] = 1 [0;1;0] = 2 [0;1;1] = 3 [1;0;0] = 4 [1;0;1] = 5
   [1;1;0] = 6 [1;1;1] = 0

   Proposition code de gray [0;0;0] = 0 [0;1;0] = 1 [0;1;1] = 2 [1;1;1] = 3
   [1;1;0] = 4 [1;0;0] = 5 [1;0;1] = 6 [0;0;1] = 7

   **)

(** 3.

    Supposons que l est un code de Gray d'ordre n-1. Alors on peut associer l à
    la liste : l = [[0;0;...;0]; [0;0;...;1]; ...; [1;0;...;0]] ou la liste l
    contient n listes, qui elles même contiennent n-1 éléments. Supposons qu'on
    concatène chaque liste de l avec un 0 devant, on a alors l1 =
    [[0;0;0;...;0]; [0;0;0;...;1]; ... [0;1;0;...;0]] De plus, en appliquant
    miroir(l), on a miroir(l) = [[1;0;...;0]; ... [0;0;...;1]; [0;0;...;0]] Puis
    en concaténant 1 devant toutes les listes contenu dans miroir(l), on a
    miroir(l) = [[1;1;0;...;0]; ... [1;0;0;...;1] [1;0;0;...;0]] Finalement, en
    concaténant l1 et miroir(l), on obtient
    [[0;0;0;...;0]; [0;0;0;...;1]; ... [0;1;0;...;0]; [1;1;0;...;0]; ...
     [1;0;0;...;1] [1;0;0;...;0]] ou toutes les listes contiennent n éléments.
    De plus on a concanténé deux liste de longeur 2^n-1. Alors, la liste final
    est de longeur 2^n-1 + 2^n-1 = 2^n. De plus, toutes listes contenu dans la
    liste finale ont un bit de différence entre chacune, d'après les
    supposittions. D'où le code de Gray.

    **)

let miroir (l : int list list) : int list list =
  let rec aux l final =
    match l with [] -> final | x :: xs -> aux xs (x :: final)
  in
  aux l []

let rec add_for_all p l : int list list =
  match l with [] -> [] | x :: xs -> (p :: x) :: add_for_all p xs

let gray (n : int) : int list list =
  let rec aux l cpt =
    if cpt <> 0 then aux (add_for_all 0 l @ add_for_all 1 (miroir l)) (cpt - 1)
    else l
  in
  aux [ [ 0 ] ] n

(** Exercice 3.8 *)

type operation = Plus | Fois | Moins
type algebre = Nombre of int | Op of operation

(** Exercice 3.9*)

let listefibo p =
  let fib = [ 1; 0 ] in
  let rec aux l =
    match l with
    | x :: y :: xs -> if x + y > p then l else aux ((x + y) :: l)
    | _ -> failwith "Impossible"
  in
  aux fib

let decomposition n =
  let fibassocie = listefibo n in
  let rec aux l cpt f =
    match l with
    | x :: xs ->
        if x + cpt < n then aux xs (x + cpt) (x :: f)
        else if x + cpt > n then aux xs cpt f
        else x :: f
    | [] -> f
  in
  aux fibassocie 0 []

type croissant = C | D | N

let rec decompose (l : int list) : int list list =
  let rec aux l cpt cr =
    match (l, cr) with
    | x :: y :: ys, N ->
        if x < y then aux ys ([ x; y ] @ cpt) (C : croissant)
        else if x > y then aux ys ([ x; y ] @ cpt) (D : croissant)
        else aux ys ([ x; y ] @ cpt) N
    | x :: y :: ys, C ->
        if x > y then [ cpt @ [ x ] ] else aux ys (cpt @ [ x ]) C
    | x :: y :: ys, D ->
        if x < y then [ cpt @ [ x ] ] else aux ys (cpt @ [ x ]) D
    | _ ->  cpt 
  in
  let rec aux2 l (p : int list) lfs =
    match l,p with
    | x::xs,[] -> aux2 (l) (aux l [] N) (aux l [] N::lfs)
    |x::xs,y::ys -> if x = y then
      aux2 xs ys lfs
    | 
    