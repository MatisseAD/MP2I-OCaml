(** Problème 1*)


(** Fonctions annexes *)

(** Fonction permettant de récupérer le n-ième terme rang d'une liste*)

let rec nieme lst n = 
  match lst with
  | [] -> failwith "n trop grand par rappot à la liste"
  | h::t -> if n = 0 then h else nieme t (n - 1);;

(** 1 *)

let diviser l = 
  let l1  = ref [] in
  let l2 = ref [] in
  for i=0 to (List.length l) -1 do
    if i < (List.length l) / 2 then 
      l1 := nieme l i :: !l1 (** Renvoie le n-ieme terme de la liste *)
  else
    l2 := nieme l i :: !l2; (** Renvoie le n-ieme terme de la liste *)
  done;
  (!l1, !l2);;


(** 2 *)

let fusion l1 l2 = 
  let l_final = ref [] in
  
  let i1 = ref 0 in
  let i2 = ref 0 in

  while !i1 < List.length l1 && !i2 < List.length l2 do
    if nieme l1 !i1 < nieme l2 !i2 then (
      l_final := nieme l1 !i1 :: !l_final;
      i1 := !i1 + 1
    ) else (
      l_final := nieme l2 !i2 :: !l_final;
      i2 := !i2 + 1
    )
  done;

  if !i1 = (List.length l1) then
    for i = !i2 to ((List.length l2) - 1) do
      l_final := nieme l2 i :: !l_final
    done
  else 
    for i = !i1 to (List.length l1) - 1 do
      l_final := nieme l1 i :: !l_final
    done;

    List.rev (!l_final);;
  
let rec tri liste = 
  if List.length liste <= 1 then liste
  else 
    let (gauche, droite) = diviser liste in
    fusion (tri gauche) (tri droite);;

(** Problème 2 *)

(** I **)

(** 1)a) *)

let echange t i j =
  begin
  let tampon = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tampon;
  end;;


(** 1)b)

On définit un invariant de boucle qui permet de prouver la validité de l'algorithme ind_min.

On pose : P(i) = "indice_min est l'indice du plus petit élément des éléments de t compris entre les indices i et i+k"

 - P(i) est vrau avant de rentrer dans la boucle.
 En effet, indice_min est initialié à i, et t.(i) est le plus petit élement de t entre i et i+k (k=0 ici).

 - On suppose que P(i) est vrai au début d'une itération de la boucle. Montre que P(i) est vrai à la fin de l'itération

 Alors, au début de l'itération l'indice du plus petit élément de t entre i et i+k est indice_min.
 A la fin de l'itération, on a comparé t.(j) avec t.(indice_min). Si t.(j) < t.(indice_min), alors on met à jour indice_min avec j, sinon on ne fait rien.
 Dans les deux cas, indice_min est bien l'indice du plus petit élément de t entre i et i+k+1. En effet, indice_min pointait, avant la comparaison sur le plus petit élement entre i et i+k, et après la comparaison, si t.(j) est plus petit, on met à jour indice_min pour qu'il pointe sur j. Or, t.(j) < t.(indice_min), donc t.(j) est bien le plus petit élément entre i et i+k+1.

Alors P(i+1) est vrai.
En sortie de boucle, on a alors effectivement le plus petit élément entre i et la fin du tableau.

*)

let ind_min t i =
  let indice_min = ref i  in
  for j = i to (Array.length t) - 1 do
    if t.(j) < t.(!indice_min) then
      indice_min := j
  done;
  !indice_min;;

(** 

1)c) 

i)

On introduit l'invariant de boucle suivant :

P(i) = "Les i premières cases du tableau contiennent les i plus petites valeurs du tableau et sont triées par ordre croissant"

La propriété est trivialement vrai pour i =0 car les 0 premières cases d'un tableau sont vides.

ii) 

Si P(i) est vraie, pour rendre P(i+1) en utilisant 'ind_min' et 'echange', il faut que, dans un premier temps, 
 - On utilise 'ind_min' pour trouver l'indice du plus petit élément du tableau entre les indices i et la fin du tableau.
 - Ensuite, on utilise 'echange' pour échanger l'élément à l'indice i avec l'élément à l'indice du plus petit élément trouvé précédemment.
  - Puis on réeffectue l'incrémentation de i, pour le faire passer à i+1 afin de garder le tableau t triée pour tout élément de 0 à i, ce qui permet de conserver l'invariant de boucle.

iii)

Pour i=Array.Length t - 1, on a bien P(i) qui signifie "Le tableau est trié par ordre croissant"

*)

(**

iv) Fonction tri select

*)

let tri_select t =
  let n = Array.length t in
  for i = 0 to n - 1 do
    let indice_min = ind_min t i in
    echange t i indice_min
  done;;

(**

v) Complexité de la fonction 'tri_select' en nombre de comparaisons entre éléments du tableau t.

Nous avons une boucle for imbriquée dans une autre boucle for.

 - La boucle for de ind_min effectue à chaque tour de boucle for de la fonction tri_select, qui varie de 0 à n-1. De même, la boucle for de ind_min varie de i à n-1. Donc on va effectuer (n - i) comparaisons pour chaque i variant de 0 à n-1.
  - Le nombre total de comparaisons est donc la somme de (n - i) pour i allant de 0 à n-1.

Ainsi => C(n) = Θ(n(n+1)/2) = Θ(n^2)

*)

(** II **)

type table = int array;;

let affiche_table table  = 
  print_string("nb d'éléments : "); 
  print_int(table.(0));
  print_string("\n");
  print_string("éléments :");
  for i=1 to table.(0) do
    print_int(table.(i));
    print_string(" ");
  done;;

let vider (t : table) =
  t.(0) <- 0;;


let ajouter t p =
  let n = t.(0) in
  t.(n+1) <- p;
  t.(0) <- n+1;;

let concatener (t1 : table) (t2 :table) =
  let nb_elements_t1 = t1.(0) in
  let nb_elements_t2 = t2.(0) in

  for i=1 to nb_elements_t2 do
    t1.(nb_elements_t1 + i) <- t2.(i);
    t1.(0) <- t1.(0) + 1;
  done;;

(** Calcul de complexité de concatener 

Soit n1=|t1| et n2=|t2| le nombre d'éléments dans t1 et t2 respectivement.

 - On a une boucle for qui va parcourir les élements de t2 de 1 à n2.
  - A chaque itération, on effectue une affectation et une incrémentation.
  - Le nombre total d'opérations est donc proportionnel à n2.
Ainsi, la complexité de concatener est Θ(n2).

*)

let max_valeurs t  = 
  let max_value = ref 0 in
  if Array.length t = 0 || t.(0) = 0 then
    -1
  else (
    for i=1 to t.(0) do
      if t.(i) > !max_value then
        max_value := t.(i)
    done;
    !max_value
  );;

let nombre_chiffres p =
  let n = ref p in
  let cpt = ref 0 in
  let power_10 = ref 10 in
  if !n = 0 then cpt := 1 else
    while !n <> 0 do
      cpt := !cpt + 1;
      n := !n / !power_10;
    done;
  !cpt;;

(**

Calcul de complexité de nombre_chiffres. Soit MAXC, le nombre maximal de chiffre de p.

Alors on va faire une boucle while qui va s'exécuter tant que n <> 0.
 - A chaque itération, on divise n par 10.
 - Le nombre d'itérations est donc proportionnel au nombre de chiffres de p, soit MAXC.

Ainsi, la complexité de nombre_chiffres est Θ(MAXC).

*)

let max_chiffres t =
  let n = t.(0) in
  if n = 0 then 0
  else (
    let mx = ref 0 in
    for i = 1 to n do
      let c = nombre_chiffres t.(i) in
      if c > !mx then mx := c
    done;
    !mx
  )
;;

(** Calcul de complexité de max chiffre

Soit le nombre de données de la table t (i.e. n = t.(0)) et maxc, le nombre maximum de chiffres parmi les données de t.

- On a une boucle for qui parcourt les n éléments de t.
- A chaque itération, on appelle la fonction nombre_chiffres qui a une complexité Θ(MAXC).
- Le nombre total d'opérations est donc proportionnel à n * MAXC.

Ainsi, c(n) = Θ(n * MAXC).
*)



let chiffre_bof p r =
  let s = string_of_int p in
  let c_r = String.get s (String.length s - r) in
  -48 + (int_of_char c_r)

let chiffre p i =

  let s = string_of_int p in

  if (String.length  s - i) < 0 then
    0
  else
    let c_i = String.get s (String.length s - i) in
    -48 + (int_of_char c_i)

let creer_baquet n =
  let baquet = Array.make 10 (Array.make n 0) in
  for i=0 to 9 do
    baquet.(i) <- Array.make n 0
  done;
  baquet;;

(** Itération 2 et 3 de r

Soit t la table t = [8, 57, 423, 50, 603, 7, 20, 453, 27...]

Itération r=1 :

baquet.(0) = [2, 50, 20, ...]
baquet.(1) = []
baquet.(2) = []
baquet.(3) = [3, 423, 603, 453...]
baquet.(4) = []
baquet.(5) = []
baquet.(6) = []
baquet.(7) = [3, 57, 7, 27...]
baquet.(8) = []
baquet.(9) = []

t = [8, 50, 20, 423, 603, 453, 57, 7, 27...]

Itération r=2 :

baquet.(0) = [1, 603, 7]
baquet.(1) = []
baquet.(2) = [3, 20, 423, 27]
baquet.(3) = []
baquet.(4) = []
baquet.(5) = [3, 50, 453, 57]
baquet.(6) = []
baquet.(7) = []
baquet.(8) = []
baquet.(9) = []

t = [8, 603, 7, 20, 423, 27, 50, 453, 57...]

Itération r=3 :

baquet.(0) = [2, 7, 20, 27, 50, 57]
baquet.(1) = []
baquet.(2) = []
baquet.(3) = []
baquet.(4) = [2, 423, 453]
baquet.(5) = []
baquet.(6) = [1, 603]
baquet.(7) = []
baquet.(8) = []
baquet.(9) = []

t = [8, 7, 20, 27, 50, 57, 423, 453, 603,...]


*)


(**

Montrons que, après l'éxécution du tri par baquets, la table t est triée par ordre croissant.

On définit alors P, un invariant de boucle tel que 

P(r) = "La table t est triée par ordre croissant sur les nombres formés des r derniers chiffres"

 - P(0) est vrai avant de rentrer dans la boucle. En effet, les nombres formés des 0 derniers chiffres sont tous égaux à 0, donc t est triée par ordre croissant sur ces nombres.

 - On suppose que P(r) est vrai au début de l'itération de la boucle pour un certain r. Montrons que P(r+1) est vrai à la fin de l'itération.

  - Après la distribution dans les baquets selon le (r+1)-ième chiffre, les nombres sont regroupés dans les baquets en fonction de ce chiffre.
  - En concaténant les baquets dans l'ordre croissant des chiffres (de 0 à 9), on obtient une table t où les nombres sont triés par ordre croissant sur le (r+1)-ième chiffre.
  - De plus, comme P(r) est supposé vrai, les nombres sont déjà triés par ordre croissant sur les r derniers chiffres.
  - Ainsi, après cette itération, t est triée par ordre croissant sur les (r+1) derniers chiffres.

Alors P(r+1) est vrai.

En sortie du programme, on a triée par ordre croissant sur les nombres formés de tous leurs chiffres, donc t est triée par ordre croissant (r=maxc).

*)

(** On prend un baquet grand pour être sur d'avoir ce qu'il faut en taille *)

let baquet = ref (creer_baquet 10000);;

let distribuer t r =
  let n = t.(0) in
  let p = ref 0 in
  let k = ref 0 in

  for i=1 to n do
    p := t.(i);
    k := chiffre !p r;
    ajouter (!baquet).(!k) !p;
  done;;

let tri_baquets t = 
  let max_nb = max_chiffres t in
  let c_t = t in 

  for i=1 to max_nb do
    distribuer c_t i;

    vider c_t;

    for j=0 to 9 do
      concatener c_t !baquet.(j)
    done;

    for j=0 to 9 do
      vider !baquet.(j)
    done;

  done;;

(**

Soit n, le nombre de données à trier (i.e. n=t.(0)) et MAXC, le nombre maximum de chiffres de données de la table.

  - On a une boucle for qui s'exécute MAXC fois (pour chaque chiffre).
  - A chaque itération, on appelle la fonction distribuer qui a une complexité Θ(n).
  - Ensuite, on a une boucle for qui s'exécute 10 fois (pour chaque baquet) et à chaque itération, on appelle concatener qui a une complexité Θ(n).
  - Enfin, on a une autre boucle for qui s'exécute 10 fois pour vider les baquets, mais cette opération est en Θ(1) car elle ne dépend pas de n.

Ainsi, la complexité totale est Θ(MAXC * n + MAXC * n) = Θ(MAXC * n).

*)

(**

14)

On suppose que la minoration du tri par baquet est de l'ordre de n (pour MAXC = 1).

Alors on la fonction identité convient.
On définit la fonction f tq 

Pour tout n appartenant à N*, f(n)=n.

 * f minore le tri par baquet
 * Son ordre de grandeur peut être attendée

*)