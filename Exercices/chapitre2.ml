(** Exercice 2.1 *)

(** 1 *)

let calcul_u f a n = 
  let ans = ref a in
  for i = 1 to n do
    ans := f(!ans);
  done;
  !ans;;

(** 2 *)

let calcul2_u (f : int * int -> int) (a : int) (b : int) (n : int) : int =
  let u_0 = ref a in
  let u_1 = ref b in
  let u_n = ref (f (a,b)) in
  for i=1 to n do
    u_0 := !u_1;
    u_1 := !u_n;
    u_n := f(!u_0, !u_1);
  done;
  !u_n;;

(** 3 *)

let calcul3 (f : 'a * 'b -> 'a) (g : 'a * 'b -> 'b) (a : 'a) (b : 'b) (n : int) =
  let u_0 = ref a in
  let v_0 = ref b in
  let u_n = ref (f(!u_0, !v_0)) in
  let v_n = ref (g(!u_0, !v_0)) in
  for i=1 to n do
    u_0 := !u_n;
    v_0 := !v_n;
    u_n := f(!u_0, !v_0);
    v_n := g(!u_0, !v_0);
  done;
  (!u_n, !v_n);;

(** Exercice 2.2 *)

let nb_inversions (a : 'a array) : int =
  let ans = ref 0 in
  for i=0 to Array.length a - 1 do
    for j=0 to Array.length a -1 do
      if i < j && a.(i) > a.(j) then
        ans := !ans + 1;
      done;
  done;
  !ans;;

type point = float * float

let euclidian_distance ((x,y) : point*point)  =
  let x1, y1 = x in
  let x2, y2 = y in
  sqrt((x1-.x2)*.(x1-.x2) +. (y1-.y2)*.(y1-.y2));;

let plusproche (t : point array) : point * point = 
  let ans = ref (euclidian_distance(t.(0), t.(1))) in
  let points = ref ((t.(0), t.(1))) in
  for i=0 to Array.length t - 1 do
    for j=0 to Array.length t - 1 do
      if euclidian_distance(t.(i), t.(j)) < !ans && i != j then
        ans := euclidian_distance(t.(i), t.(j));
        points := (t.(i), t.(j));
    done;
  done;
  !points;;


let inserer t value = 
  let new_t = Array.append t (Array.make 1 value) in
  let n = ref (Array.length new_t - 2) in
  while !n >= 0 && value < new_t.(!n) do
    let tampon = new_t.(!n) in
    new_t.(!n) <- value;
    new_t.(!n+1) <- tampon;
    n := !n - 1;
  done;
  new_t;;

(** 

Complexité de la fonction :

Soit n, le nombre de comparaison 

C(n) = O(n)

Pire des cas : 

La valeur insérer est la plus petite et est insérer en position t.(pos + 1).

Donc, C(n) = Θ(n) . On effectue n comparaisons

Meilleur des cas :

C(n) = Θ(1) <- On effectue 1 seule comparaison

La valeur insérer est la plus grande du tableau et est insérer en position t.(pos + 1)

**)

let triInsertion t =
  let new_t = ref (Array.make 1 t.(0)) in
  for i=1 to Array.length t - 1 do
    new_t := inserer !new_t t.(i)
  done;
  !new_t;;

(** 

Dans le pire des cas, la liste passé en paramètre est trié dans l'ordre inverse.
Alors la fonction inserer est en C(n) = Thêta(n), où n est le nombre de comparaisons. 
On fait cela m fois, où m est la taille du tableau t.
Donc la complexité totale est C(n) = Thêta(n^2)

Dans le meilleur des cas, le tableau est déjà trié, alors il y a une seule comparaisons à chaque fois pour un tableau de taille n.
Donc, c(n) = Thêta(n).

H' = "k est un indice valide de t tel que t.(0..k) est une partie de la liste déjà triée"
H' = "La liste new_t est trié à chaque tour de boucle"

Ce tri semble stable car on insère les éléments dans le même ordre qu'ils apparaissent dans le tableau d'origine.


**)

(** Exercice 2.3 *)

(** 

Soit a >= b.
Soit n,p deux entiers. On pose H = " n>=p>=0 et pgcd(n,p)=pgcd(a,b)"

1)a) 

Pour avoir H vraie, il suffit de donner n=a et p=b.

1)b)

Supposons que p=0.

Alors pgcd(n,p) = n.

1)c)

Une modification de l'environnement à faire est d'affecter à n la valeur suivante : 

n <- n-p

*)

let pgcd (a:int) (b:int) : int =
  let a_mut = ref a in
  let b_mut = ref b in

  while a_mut >= b_mut do
    a_mut := !a_mut - !b_mut;
  done;
  
  if !a_mut = 0 then !b_mut else !a_mut;;

let pgcd_euclide (a:int) (b:int) : int =
  let n = ref a in
  let p = ref b in

  while !n mod !p != 0 do
    let reste = !n mod !p in
    n := !p;
    p := reste;
  done;

  !p;;