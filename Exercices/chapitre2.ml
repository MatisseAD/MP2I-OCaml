(** Exercice 2.1 *)

(** 1 *)

let calcul_u f a n =
  let ans = ref a in
  for i = 1 to n do
    ans := f !ans
  done;
  !ans

(** 2 *)

let calcul2_u (f : int * int -> int) (a : int) (b : int) (n : int) : int =
  let u_0 = ref a in
  let u_1 = ref b in
  let u_n = ref (f (a, b)) in
  for i = 1 to n do
    u_0 := !u_1;
    u_1 := !u_n;
    u_n := f (!u_0, !u_1)
  done;
  !u_n

(** 3 *)

let calcul3 (f : 'a * 'b -> 'a) (g : 'a * 'b -> 'b) (a : 'a) (b : 'b) (n : int)
    =
  let u_0 = ref a in
  let v_0 = ref b in
  let u_n = ref (f (!u_0, !v_0)) in
  let v_n = ref (g (!u_0, !v_0)) in
  for i = 1 to n do
    u_0 := !u_n;
    v_0 := !v_n;
    u_n := f (!u_0, !v_0);
    v_n := g (!u_0, !v_0)
  done;
  (!u_n, !v_n)

(** Exercice 2.2 *)

let nb_inversions (a : 'a array) : int =
  let ans = ref 0 in
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      if i < j && a.(i) > a.(j) then ans := !ans + 1
    done
  done;
  !ans

type point = float * float

let euclidian_distance ((x, y) : point * point) =
  let x1, y1 = x in
  let x2, y2 = y in
  sqrt (((x1 -. x2) *. (x1 -. x2)) +. ((y1 -. y2) *. (y1 -. y2)))

let plusproche (t : point array) : point * point =
  let ans = ref (euclidian_distance (t.(0), t.(1))) in
  let points = ref (t.(0), t.(1)) in
  for i = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 1 do
      if euclidian_distance (t.(i), t.(j)) < !ans && i != j then
        ans := euclidian_distance (t.(i), t.(j));
      points := (t.(i), t.(j))
    done
  done;
  !points

let inserer t value =
  let new_t = Array.append t (Array.make 1 value) in
  let n = ref (Array.length new_t - 2) in
  while !n >= 0 && value < new_t.(!n) do
    let tampon = new_t.(!n) in
    new_t.(!n) <- value;
    new_t.(!n + 1) <- tampon;
    n := !n - 1
  done;
  new_t

(** Complexité de la fonction :

    Soit n, le nombre de comparaison

    C(n) = O(n)

    Pire des cas :

    La valeur insérer est la plus petite et est insérer en position t.(pos + 1).

    Donc, C(n) = Θ(n) . On effectue n comparaisons

    Meilleur des cas :

    C(n) = Θ(1) <- On effectue 1 seule comparaison

    La valeur insérer est la plus grande du tableau et est insérer en position
    t.(pos + 1)

    **)

let triInsertion t =
  let new_t = ref (Array.make 1 t.(0)) in
  for i = 1 to Array.length t - 1 do
    new_t := inserer !new_t t.(i)
  done;
  !new_t

(** Dans le pire des cas, la liste passé en paramètre est trié dans l'ordre
    inverse. Alors la fonction inserer est en C(n) = Thêta(n), où n est le
    nombre de comparaisons. On fait cela m fois, où m est la taille du tableau
    t. Donc la complexité totale est C(n) = Thêta(n^2)

    Dans le meilleur des cas, le tableau est déjà trié, alors il y a une seule
    comparaisons à chaque fois pour un tableau de taille n. Donc, c(n) =
    Thêta(n).

    H' = "k est un indice valide de t tel que t.(0..k) est une partie de la
    liste déjà triée" H' = "La liste new_t est trié à chaque tour de boucle"

    Ce tri semble stable car on insère les éléments dans le même ordre qu'ils
    apparaissent dans le tableau d'origine.

    **)

(** Exercice 2.3 *)

(** Soit a >= b. Soit n,p deux entiers. On pose H = " n>=p>=0 et
    pgcd(n,p)=pgcd(a,b)"

    1)a)

    Pour avoir H vraie, il suffit de donner n=a et p=b.

    1)b)

    Supposons que p=0.

    Alors pgcd(n,p) = n.

    1)c)

    Une modification de l'environnement à faire est d'affecter à n la valeur
    suivante :

    n <- n-p *)

let pgcd (a : int) (b : int) : int =
  let a_mut = ref a in
  let b_mut = ref b in

  while a_mut >= b_mut do
    a_mut := !a_mut - !b_mut
  done;

  if !a_mut = 0 then !b_mut else !a_mut

let pgcd_euclide (a : int) (b : int) : int =
  let n = ref a in
  let p = ref b in

  while !n mod !p != 0 do
    let reste = !n mod !p in
    n := !p;
    p := reste
  done;

  !p

(** Exercice 2.5 **)

let basebversbase10 (b : int) (tn : int array) : int =
  let ans = ref 0 in
  let n = Array.length tn in
  for i = 0 to n - 1 do
    ans := (!ans + tn.(n - 1 - i)) * b
  done;
  !ans

(** Lorsque n est connu, l'algorithme est comme suit, on effectue la division
    euclidienne de n par la base b, puis on insère le reste de cette division
    dans le tableau. Ensuite, on effete de même avec le quotient de cette
    division. Pour déterminer p, on compte le nombre de fois qu'on doit
    effectuer la divison euclidienne avant d'avoir un quotient égal à 0.*)

let base10versbaseb (b : int) (n : int) =
  let tn = ref [||] in
  let q = ref 0 in
  let r = ref 0 in
  while !q <> 0 do
    r := n mod b;
    q := n / b;
    tn := Array.append !tn [| r |]
  done;
  !tn

(** Complexité, boucle while -> C(n) = O(n), où C est le nombre d'affectation **)

let somme (tx : int array) (ty : int array) (b : int) =
  let ans = ref [||] in
  let n = min (Array.length tx) (Array.length ty) in
  for i = 0 to n - 1 do
    if tx.(i) + ty.(i) > b then (
      let q = (tx.(i) + ty.(i)) / b in
      let r = (tx.(i) + ty.(i)) mod b in
      ans := Array.append !ans [| r |];
      ans := Array.append !ans [| q |])
    else if Array.length !ans < i + 1 then
      ans := Array.append !ans [| tx.(i) + ty.(i) |]
    else !ans.(i) <- !ans.(i) + tx.(i) + ty.(i)
  done;
  !ans

(** 2B Programmation fonctionnelle *)

(**Exercice 2.2**)

(* 1) H(n) = "fn termine et renvoie true si n est paire, false si n est impaire et gn true si n est impair et false si n est pair"*)

(* 2) H(n) = "fn  termine et renvoie 2n+1 et gn termine et renvoie 2n"*)

(** Exercice 2.3 *)

let succ x = x + 1
let pred x = x - 1
let rec add x y = if y = 0 then x else add (succ x) (pred y)

let mult x y =
  let rec aux x y =
    match (x, y) with p, 0 -> p | a, b -> add a (aux a b - 1)
  in
  aux x y

(** Exercice 2.9 *)

let rec calcul_vn a b c n =
  match n with
  | 0 -> a
  | 1 -> b
  | 2 -> c
  | x ->
      sqrt
        (calcul_vn a b c (x - 1)
        *. calcul_vn a b c (x - 3)
        *. calcul_vn a b c (x - 2))

(** 2)

    Plus généralement, on considère une suite u définit par récurrence (suite
    récursive) d'ordre p. On a donc une complexité exponentielle, => Abre
    récursif *)

(**Exerice 2.26 **)

let inversion (t : int array) =
  let cpt = ref 0 in
  for i = 0 to Array.length t - 1 do
    for j = i to Array.length t - 1 do
      if t.(i) > t.(j) then incr cpt
    done
  done;
  !cpt

(** Corrextion exercice 2.26 **)

(**

Méthode naïve : Double boucle for comme réalisé au dessus, en O(n²).

Méthode diviser pour régner : Principe du tri fusion

On trie et on calcule en même temps le nb d'inversion

"Diviser"
  - On divise le tableau en 2 tableaux quasi de même longeur
  - On calcul récursivement le nb d'inversion dans la partie, le nb d'inversions dans la partie droite et on trie chaque moitié
"Régner"; Exemple :

On considère le tableau suivant : [|1;3;7;10;18||;2;2;6;11|]
(Où la barre || représente la séparation des deux tableaux)

On adopte la fonction de fusion utilisée dans le tri fusion

On utilise g,d des indices pour indiquer la 1ère case non encore mise dans le tableau trié de chaque motiié.

  ° Si t.(g) <= t.(d)
  On récupère t.(g), on le met dans le tableau trié, on incrémente t.(g) (Le nombre d'inversion est inchangée)
  ° Si t.(d) < t.(g)
  On récupère t.(d), on le met dans le tableau trié, on incrémente d (Le nombre d'inversio, est augmenté de la taille restante de la moitié de gauche)

Donc, on a pour tableau final :
[1;2;2;3;6;7;10;11;18]
avec pour compteur :
cpt = 0 + 4 + 4 + 0 + 3 + 0 +0 + 1 + 0 => total d'inversions entre moitié gauche et moitié droite

"Spécificité d'une telle fonction récursive"

  - Cas d'arrêt : On a un tableau de longeur 0 ou 1 (nb d'inversions=0 ; tri = rien à faire)

  - Indication de programmation : On utilise une fonction auxiliare récursive qui tri la zone du tableau t[i;j[ et renvoie le nb d'inversions total dans cette zone du tableau
*)

let fusion arr1 arr2 =
  let cpt = ref 0 in

  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  let result = Array.make (n1 + n2) 0 in
  let i = ref 0 in
  let j = ref 0 in
  let k = ref 0 in
  while !i < n1 && !j < n2 do
    if arr1.(!i) < arr2.(!j) then (
      incr cpt;
      result.(!k) <- arr1.(!i);
      incr i
    ) else (
      result.(!k) <- arr2.(!j);
      incr j
    );
    incr k
  done;
  while !i < n1 do
    result.(!k) <- arr1.(!i);
    incr i;
    incr k
  done;
  while !j < n2 do
    incr cpt;
    result.(!k) <- arr2.(!j);
    incr j;
    incr k
  done;
  (result,cpt)

let div arr =
  let n = Array.length arr in
  let mid = n / 2 in
  let left = Array.sub arr 0 mid in
  let right = Array.sub arr mid (n - mid) in
  (left, right)

let rec inversion t =
  let rec aux t cpt =
    if Array.length t = 0 || Array.length t = 1 then
      (t,0)
    else
      let a,b = div t in
      let c = aux a cpt in
      let d = aux b cpt in
      let e,f = fusion (fst c) (fst d) in
      (e,!f)
    in aux t 0



(** Exerice 2.27 **)

(** Dans un échéquier n*n, on cherche à placer n reines de sorte qu'aune reine
    ne menace une autre. (Une reine mecane une entre si elles sont sur la même
    diagonale, la même horizontale) On doit décier pour chaque ligne i du n° de
    la colonne où met la reine de la ligne i. On note ce n° t.(i)

    "Objectif" : Fabriquer le tableau [|t.0,...,t.(n-1)|] On doit résoudre un
    problème (P) qui se résume en la résolution d'une succesion de questions
    Q(0),..,Q(n-1) où Q(i) = "Où est-ce que je mets la reine de la ligne i ?"

    "Spécificité d'une telle fonction récursive" On peut utiliser une fonction
    auxiliaire :

    solution_a_partir_de i res

    qui ajoute à res toutes les solutions qui prolongent la valeur courante de
    [|t.(0),..,t.(n-1)|]

    "Pseudo code" :

    let reine (n : int) = let t = Array.make n (-1) in let solution_a_partir_de
    i res = if i = n then (** On a répondu à Q(0), ..., Q(n-1)**) res :=
    (Array.copy t) :: (!res) else for j=0 to n-1 do if not en_prise i j then
    begin t.(i) <- j; solution_a_partir_de (i+1) res end

    let toutes_sol_reines n = let t = Array.make n (-1) in let sol = ref [] in
    let solution_a_partir_de i res =

    in solution_a_partir_de 0 sol

    **)

(**Exercice 2.25 *)

(**Elements majoritaires *)

let majoritaire (t : 'a array) : bool * 'a * int =
  (** Variables de réponses "finales" *)
  let ans_bool = ref false in
  let value_ocur = ref (-1) in
  let nb_ocur = ref 0 in
  (**Fin de variables réponses finales*)
  let n = Array.length t in
  let max_v = ref t.(0) in
  (**H(0) est vrai, le tableau nul ne possède pas de vlauer*)
  for i=0 to n - 1 do
    (** Supposons H(i)*)
    if t.(i) > !max_v then
      max_v := t.(i)
    (** H(i+1) est vrai*)
  done;
  let t_aux = Array.make !max_v 0 in
  for i=0 to n-1 do
    (** Supposons H(i)*)
    t_aux.(t.(i)-1) <- t_aux.(t.(i)-1) + 1;
    (** H(i+1) est vrai*)
  done;
  let i = ref 0 in
  (** La boucle while termine, i est un variant de boucle, c'est un entier, majoré, qui est incrémenter de 1 à chaque itération.*)
  while not (!ans_bool) && i < max_v do
    (** Supposons H(i)*)
    if t_aux.(!i) > n/2 then
      ans_bool := true;
      value_ocur := !i;
      nb_ocur := t_aux.(!i);
    incr i;
    (** H(i+1) est vrai*)
    done; 
    if !ans_bool then
      (!ans_bool, !value_ocur +1, !nb_ocur)
    else
      (!ans_bool,-1,0)

(** Montrons que cette fonction termine et renvoie bien la bonne valeur 

H(i) : "La variable 'ans_bool' est vrai si le tableau t_aux possède une valeur supérieur à n/2, faux sinon"

*)

(** Complexité C(n) où n est la taille du tableau t 

1ère boucle for :

C(n) = O(n)

2ème boucle for allant de 0 jusqu'au max de la valeur contenu dans le tableau t

C(n) = O(max(t)) où max représente la valeur maximale

3ème boucle while

C(n) = O(max(t))

Donc,

C(n) = 2O(max(t)) + O(n)

Si max(t) > O(n),

C(n) = O(max(t))

Sinon,

C(n) = O(n)


*)

(**Autres méthodes possibles pour 

1. Naif : maximum de la liste du nombres d'occurences, O(n^2)

2. Pour un tableaux d'entiers : on crée le tableau des occurences des elts de t
  - 1° parcours pour trouver max(t)
  - 2° on crée un tableau occ de taille max(t)+1
  - 3° on reparcourt t et on incrémente la case occ.(t.(i)) pour tout 0 <= i <= len(t) -1
 O(n+max(t)). Adaptable aux types de donnees avec un dctionnaire associant objets et indice entier

3.  On trie le tableaupuis avec un parcours on recherche ensuite l'elt majoritaire en O(n*ln(n))

4. METHODE DE BOYER MOORE A RETENIR

Rque : si a et b st deux elts de tq a != b :
Rechercher un elt maj de t <-> rechercher u nelt maj dans le tableau obtenu 
à partir de t en supprimant a et b (ie les paires distinctes)

ALGO: On parcourt t
  - Si t.(i) = sommet de la pile // pile est vide -> on empile t.(i)
  - Sinon : on dépile un elt de la pile
Si à la fin la pile est non vide, le sommet est de la pile est le seul majoirtaire possible

/!!\ L'elt restant n'est pas forcement majoritaire (ex : plusieurs elts qui occurent le mm nb de fois)
MAIS si il y a un elt maj, c'est le seul qui resiste et reste present au final dans la pile.
Il reste à tester son nb d'occurences.

Implémentation :
pour coder la pile il nous suffit de connaitre la variable h = hauteur de la pile; cord = seule valeur qui est dans la pile
Iteration :
  - si h = 0 alors h devient 1 et cord devient t.(i)
  - si h != 0 et t.(i) = cord alors h devient h+1
  - si h != 0 et t.(i) != cord alors h devient h-1
 
=> complexite O(n)

**)
