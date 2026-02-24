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

(** Exercice 2.5 **)

let basebversbase10 (b :int) (tn : int array) : int =
  let ans = ref 0 in
  let n = Array.length tn in
  for i=0 to n - 1 do
    ans := (!ans + tn.(n - 1 - i)) * b;
  done;
  !ans;;

(** Lorsque n est connu, l'algorithme est comme suit, on effectue la division euclidienne de  n par la base b, puis on insère le reste de cette division dans le tableau. Ensuite, on effete de même avec le quotient de cette division. Pour déterminer p, on compte le nombre de fois qu'on doit effectuer la divison euclidienne avant d'avoir un quotient égal à 0.*)

let base10versbaseb (b :int) (n : int) =
  let tn = ref [||] in
  let q = ref 0 in
  let r = ref 0 in
  while !q <> 0 do
    r := n mod b;
    q := n / b;
    tn := Array.append !tn [|r|];
  done;
  !tn;;

(** Complexité, boucle while -> C(n) = O(n), où C est le nombre d'affectation **)

let somme (tx : int array) (ty : int array) (b : int) =
  let ans = ref [||] in
  let n = min (Array.length tx) (Array.length ty) in
  for i=0 to n-1 do
    if tx.(i) + ty.(i) > b then (
      let q = (tx.(i) + ty.(i)) / b in
      let r = (tx.(i) + ty.(i)) mod b in
      ans := Array.append !ans [|r|];
      ans := Array.append !ans [|q|]
    ) else (
      if Array.length (!ans) < i+1 then
        ans := Array.append !ans [|tx.(i) + ty.(i)|]
      else
        (!ans).(i) <- (!ans).(i) + tx.(i) + ty.(i)
    )
  done;
  !ans;;

(** 2B Programmation fonctionnelle *)

(**Exercice 2.2**)

(* 1) H(n) = "fn termine et renvoie true si n est paire, false si n est impaire et gn true si n est impair et false si n est pair"*)

(* 2) H(n) = "fn  termine et renvoie 2n+1 et gn termine et renvoie 2n"*)

(** Exercice 2.3 *)

let succ x = x +1;;
let pred x = x -1;;

let rec add x y =
  if y = 0 then
    x
  else
    add (succ x) (pred y)

let mult x y =
  let rec aux x y =
    match x,y with
    | p,0 -> p
    | a,b -> add a (aux a b-1)
  in aux x y


(** Exercice 2.9 *)

let rec calcul_vn a b c n =
  match n with
  | 0 -> a
  | 1 -> b
  | 2 -> c
  | x -> sqrt((calcul_vn a b c (x-1)) *. (calcul_vn a b c (x-3)) *. (calcul_vn a b c (x-2)))

(**

2)

Plus généralement, on considère une suite u définit par récurrence (suite récursive) d'ordre p.
On a donc une complexité exponentielle, => Abre récursif

*)

