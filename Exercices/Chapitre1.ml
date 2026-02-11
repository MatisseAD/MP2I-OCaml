(** Exercice 14 *)

let mystere (a,b) = let q = a/b in a - b*q;;

(** La fonction mystère effectue la division euclidenne de a par b. La valeur "q" représente le quotient de la divison et a - b 'fois' q représente le reste de la divison*)

let est_paire(x) = if mystere(x,2) == 0 then true else false;;

(** Exercice 15*)

(** let f x = if x =0. then print_string("Je ne peux pas diviser par 0") else 1./.(x*x);; **)

(** La fonction émet une erreur car elle ne renvoit pas le même type selon la condion booléen passé en paramètre*)

exception DivisionParZeroCestPasBien;;

let f x = if x = 0. then raise DivisionParZeroCestPasBien else 1. /. (x*.x);;

let f_prime x = if x = 0. || x = 1. then raise DivisionParZeroCestPasBien else 1. /. (x *. (x-.1.));;

(** Exercice 16*)

let f1 z = 
  match z with
  | (0,_) -> 0;
  | (_,0) -> 1;
  | (x,y) when x = y -> 2;
  | (x,y) -> x;;

(** La fonction fait un pattern matching, si le doublet passé en paramètre commence par un 0, on renvoit 0, si elle termine par un 0 on renvoit un. Soit x,y appartent à K, si x = y alors la fonction renvoit 2, sinon elle renvoit x.*)

f1 (4,0);;
f1 (0,3);;
f1 (5,5);;
f1 (4,6);;

let g z =
  match z with
  | (x,y) when 2. *. (x *. 2.) +. y *. y = 1. -> 0.
  | (x,y) when 2. *. (x *. 2.) +. y *. y < 1. -> 1.
  | (x,y) when 2. *. (x *. 2.) +. y *. y > 1. -> x +. y
  | _ -> 0.

(** Exercice 17 *)

let u = function (n,deb,f) -> 
  let temp = ref deb in
  begin 
    for i = 0 to n do temp := f(!temp) done;
    !temp
  end;;

(** 
n : int
deb : alpha
f : fonction alpha -> alpha
temps : alpha -> alpha ref

Cette fonction calcul de le nième terme de la suite u par la fonction f
*)
let f x = 5*x+3;;

u (9, 5, f);;

(** Il faut faire attention à la valeur du rang*)

(** Exercice 18 *)
let base_to_dec (s: string) (b: int) : int =
  let power = ref 1 in
  let ans = ref 0 in
  for i=String.length s - 1 downto 0 do
    ans := !ans + int_of_string(String.make 1 (String.get s i)) * !power;
    power := !power * b;
  done;
!ans;;

(** Exercice 19 *)

type complexe = {mutable réel: float; mutable imaginaire: float};;

let z1 : complexe = {réel= 2.; imaginaire=3.};;

let z2 : complexe = {réel=5.; imaginaire=7.};;

(** Modification de z *)

z1.réel <- 5.;;

let add u v =
  let z3 = {réel=0.; imaginaire=0.} in
  begin
    z3.réel <- u.réel +. u.imaginaire;
    z3.imaginaire <- u.imaginaire +. u.réel;
  end;;

let mult u v =
  let z3 = {réel=0.; imaginaire=0.} in
  begin
    z3.réel <- u.réel *. v.réel +. u.imaginaire *. v.imaginaire;
    z3.imaginaire <- u.réel *. v.imaginaire +. u.imaginaire *. v.réel;
  end;;

let conjuge u = let z3 = {réel=0.; imaginaire=0.} in
  begin
    z3.réel <- u.réel;
    z3.imaginaire <- u.imaginaire *. -1.;
  end;;

let modul u = sqrt(u.réel *. u.réel +. u.imaginaire *. u.imaginaire)



(** Exercice 23 *)
type genre = M |F
type personne = {nom: string; prenom: string; age: int; sexe: genre} 
type classe = personne array ;;

(** Exercice 24 *)

type nombre = Entier of int | Rationnel of int*int | Reel of float | Complexe of float*float

let rec print_all_elements list = 
  match list with
  | [x] -> 
    print_string(" ");
    print_int(x);
  | h :: t ->  
    print_string(" ");
    print_int(h);
    print_all_elements(t);
  | [] -> print_string("")

let rec print_all_nombre list = 
  match list with 
  | [] -> print_newline()
  | h :: t -> 
    print_all_nombre(t);
    print_string(" ");
    match h with
    | Entier x -> print_int(x)
    | Rationnel (num, den) -> 
      print_int(num); print_string("/"); print_int(den);
    | Reel x -> print_float(x);
    | Complexe (real, imaginary) -> print_float(real); print_string(" + i"); print_float(imaginary);


(**let rec sommation liste f =
  match liste with
  | [] -> 0
  | h :: t -> f h (sommation t f)**)

(** Exercice 25 *)

type arbre = Feuille of int | Noeud of int*arbre*arbre;;

let rec minimum (tree : arbre) =
  match tree with
  | Noeud (x,f1,f2) -> 
    if x <= minimum f1 && x <= minimum f2 then
      x
    else
      if minimum f1 < minimum f2 then
        minimum f1
      else
        minimum f2
  | Feuille x -> x 

let rec sommearbre (tree : arbre) : int =
  match tree with
  | Feuille x -> x
  | Noeud (x, f1, f2) -> x + sommearbre(f1) + sommearbre(f2)