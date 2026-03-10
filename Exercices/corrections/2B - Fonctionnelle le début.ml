	else  (aux (deb +. dx) (res +.  (dx *. (phi deb))) )
	in aux  a 0.;;
	

integrale (fun x-> (4.)/.( x*.x +. 1.)) 0. 1. 0.001;;



(* calcul de la somme des f(k) de a à b*)


let somme f a b =
	let rec aux f a b res=
	(* calcul de res + somme des f(k) de a à b*)
		match b-a with
		k when k<0 ->  res 
		|_ -> aux f (a+1) b (res + (f a))
	in aux f a b 0;;
	
	
somme (fun x-> x*x) 0 10;;10*(10+1)*(2*10+1)/6;;



(* On détermine si un entier est dans la suite de Fibonacci*)



let dansFibo x=
	let rec aux x a b=
	   (* regarde si x est dans la suite de premiers termes a et b tq 
	   u_(n+2)=u_(n+1)+u_n*)
		match x-a with 
		0 -> true
		|k when k<0 -> false
		|_ -> aux x b (a+b)
	in aux x 0 1;;
	
	
(*verification*)

let rec  fibo_liste n =
match n with 
0 -> [0]
|1-> [1;0]
|_-> let a::b::q = fibo_liste (n-1) in 
    (a+b)::a::b::q;;
    
fibo_liste 15;;


let rec est_dans x l =
	match l with 
	[] -> false
	|t::q -> (x = t) || (est_dans x q);;

dansFibo 233;;
    
dansFibo 544;;

x;;




(*******************************************************)
(*****************Exercice 9****************************)
(*******************************************************)







(***********Exos 9: Question 3, suite récurrentes d'ordre beaucoup*****************)

(* approche impérative: rappels*)


let v_imperative n = 
	let tab = Array.make  (n+1) 0 in 
	tab.(0) <- 1; 
	for i = 0 to (n-1) do 
		for k = 0 to i do 
			tab.(i+1) <- tab.(i+1) + tab.(k)* tab.(i-k)
		done
	done;
	tab.(n);;
		
		
(*test*)
v_imperative 5;;
v_imperative 100;; (* c'est négatif car????*)
 
(* Rq: la complexité est quadratique*)


(* avec une approche purement fonctionnelle*)


(* calcul de res +  produit scalaire de deux vecteurs codés sous forme de liste d'entiers*)

let rec prod_scal li1 li2 res  =
	match li1, li2 with 
	[], [] -> res 
	|t1::q1, t2::q2 ->  prod_scal q1 q2 (res + t1*t2)
	|_ -> failwith("pb de taille")
	
(* fonction de calcul de la somme de u_ku_(n-k) lorsque les u_k 
sont dans la liste *)

let somme_mixee liste =
	prod_scal (List.rev liste) liste 0;;
	
	
let ajoute_som_mixee liste=
	List.cons (somme_mixee liste) liste;;
	
let v n =
   (* fonction de calcul de la liste [v_(n),...,v_(0)]*)
	let rec suite_v n = 
		match n with 
		0 -> [1]
		|_ -> ajoute_som_mixee (suite_v (n-1))
	in List.hd (suite_v n);;
	
	
v 5;;
v 100;;
	





(**** Correction by phA*********)





 let v n =
  let rec phi (lst : int list) (lr : int list) =
    match (lst, lr) with
    | x :: xs, y :: ys -> (x * y) + phi xs ys
    | _ -> 0
  in
  let rec aux n =
    match n with
    | 0 -> [ 1 ]
    | _ ->
        let k = aux (n - 1) in
        phi k (List.rev k) :: k
  in
  List.hd (aux n);;

(*******************************************************)
(*****************Exercice 10****************************)
(*******************************************************)


(** fonction d'itération***)





let rec iterer f n=
match n with 
0-> (fun x -> x)
|_-> (fun x -> (iterer f (n-1)) (f(x)));;


iterer (fun x -> x+1) 1000000;;

(iterer (fun x -> x+1) 1000000) 0;;
(* pas de stack overflow*)


let rec iterer2 f n=
match n with 
0-> (fun x -> x)
|_-> (fun x -> f ( (iterer2 f (n-1)) x));;


(iterer2 (fun x -> x+1) 1000000) ;;
(*pas de stackoverflow*)

(iterer2 (fun x->x+1) 1000000) 0;;
(* fait un stack overflow*)



(*application au calcul de fibonacci*)


let fibo n=
fst (iterer (fun (x,y)-> (y, x+y)) n (0,1));;


fibo 7;;







(*******************************************************)
(*****************Exercice 11****************************)
(*******************************************************)




let expo2 (x:float) (n:int): float =
	let rec aux y k res = 
	(* renvoie y^k*res *)
		match k with 
		0 -> res
		|_ -> let q,r = (k/2), (k mod 2) in 
		if r = 0 then (aux (y *. y) q  res) else 
		(aux (y *. y) q  (y*.res))
	in aux x n 1.;;
	
	
expo2 2. 10;;
	



let expo3 (x:float) (n:int): float =
	let rec aux y k res = 
	(* renvoie y^k*res *)
		match k with 
		0 -> res
		|_  when k mod 3 = 0 -> let q = (k/3) in (aux (y *. y *. y) q  res)
		|_  when k mod 3 = 1 -> let q = (k/3) in (aux (y *. y *. y) q  (y*.res) )
		|_  (* k mod 3 = 2*) -> let q = (k/3) in (aux (y *. y *. y) q  (y*.y*.res) )
	in aux x n 1.;;

expo3 2. 10;;



(*******************************************************)
(*****************Exercice 12****************************)
(*******************************************************)





(* récursivité sur la taille du pb *)



let rec somme_naive t j=
	match j with
	-1-> 0
	|_-> t.(j)+(somme_naive t (j-1));;
	
let sommNaive t =
    let der = Array.length t -1 in 
    somme_naive t der;;

let rec somme_aux t  j res = 
	match j with 
	-1-> res ; 
	|_ -> somme_aux t (j-1) (res+t.(j));;
	
	
somme [|1;2;3|] 1 0;;


let somme_term t = 
	let der = (Array.length t) -1 in 
	somme_aux t der 0;;
	
	
somme_term [|1;2;3|];;

sommNaive (Array.make 500000 2);;(*stack overflow
 during evaluation (looping recursion? *)

somme_term (Array.make 500000 2);; (* réponse 1000000 c'est pourtant 
pas difficile*)



(* maximum des éléments d'un tableau d'un entier *)


let rec maxi_aux t j res= (* max des cases de 0 à j*)
   let pg x y =
   if y> x then y else x in 
	match j with 
	0-> res;
	|_-> maxi_aux t (j-1) (pg t.(j) res);;
	
let max_tab t=
	let der =  Array.length t -1 in 
	maxi_aux t der t.(0);;


max_tab [|15;12;10;25|];;


(* dichotomie récursive*)

(*recherchons x entre les cases d'indice deb et fin *)
let rec laoupala t  x deb fin=
	if fin <= deb then t.(deb) = x 
	else 
		begin 
		let milieu = (deb+fin)/2  in 
		 if t.(milieu)<  x then
		 laoupala t x (milieu+1) fin
		 else 
		 laoupala t x deb milieu;
		end;;
		 
	

laoupala [|1;3;5|] 1 0 2;;


let est_present t x =
	laoupala t x 0 (Array.length t -1);;
	
	
est_present [|2;4;5|] 5;;




(***********recherche d'anagrammes ****************)


(*** la bonne méthode serait: on trie les tableaux, puis on compare vérifie que
les mots sont égaux*)



(* mauvaise méthode: on regarde si la dernière de t1 est dans t2, si oui 
on échange dans t2 la dernière case avec celle contenant la dernière de t1 et on poursuit
récusrivement sur les tableaux obtenus à partir de t1 et t2, 
en enlevant une case.*)



let est_present t i x = (* renvoie -1 si x n'est pas ds la tableau avant la case 
i et l'indice de case sinon*)
      let res = ref (-1) and k = ref 0 and cond = ref true in
      while  !cond do
           	if t.(!k) = x then 
           		begin 
           		res := !k; 
           		cond := false;
           		end
           	else
           		k:= ! k +1;
           	if !k > i then cond := false;
       done;
       !res;; 
       
est_present [|12;15;26;32|] 2 27;;



let echange t i j= 
	let a = t.(i) and b = t.(j) in 
	begin
	t.(i)<- b;
	t.(j)<-a;
	end;;



(*version itérative*)
let anagramme t tp= (* prérequis t et tp sont de même longueur*)
	let lg = Array.length t  in
	let res = ref true   and i =  ref (lg -1) and cond = ref true in 
	while !cond do
		let u = est_present tp !i t.(!i) in 
		if u = (-1) then 
			begin
			res :=false;
			cond := false;
			end
		else
			begin
			echange tp !i u;
			i := (!i)-1; 
			if !i<0 then cond := false;
			end;
	done;
	!res;;
	






(*******************************************************)
(*****************Exercice 13****************************)
(*******************************************************)






(******récusrivité sur la talle d'un pb chaines de caractères********)



let rec est_prefixe wp w=
let lwp = String.length wp and lw= String.length w in 
match lwp with 
0-> true;
|_->   if String.get wp 0  <> String.get w 0  then false
     else  
   est_prefixe (String.sub wp  1 (lwp-1)) (String.sub w  1 (lw-1));;
   
   
est_prefixe "bone" "bonjour";;
est_prefixe "bon" "bonjour";;


let rec est_suffixe wp w=
let lwp = String.length wp and lw= String.length w in 
match lwp with 
0-> true;
|_->   if String.get wp (lwp-1)  <> String.get w (lw-1)  then false
     else  
   est_suffixe (String.sub wp  0 (lwp-1)) (String.sub w  0 (lw-1));;
   
   
est_suffixe "jour" "bonjour";;
est_suffixe "tour" "bonjour";;



let rec  est_facteur wp w = 
let lwp = String.length wp and lw= String.length w in 
if lwp > lw  then false
else 
	if est_prefixe wp w then true
	else est_facteur wp (String.sub w  1 (lw-1));;


est_facteur "com" "pas commutatif";;



(* commençons par écrire une fonction récursive qui ajoute au début res les 
le miroir de w*)
let rec aux w res=
let lw = String.length w in 
match w with 
""-> res;
|_ ->   aux (String.sub w 1 (lw-1)) ((String.sub w 0 1)^res);;


let miroir w= aux w "";;


miroir "papi";;






(*******************************************************)
(*****************Exercice 14****************************)
(*******************************************************)





(*****************calcul des combinaisons***************)


let rec comb n p=
match (n,p) with 
(0, 0) -> 1
|(0, _) -> 0
|(k,p) when p>k -> 0
|(k,q) -> (comb (n-1) (p-1))+ (comb (n-1) p);;


comb 3 2;;


(comb 9 3) = (9*8*7)/(3*2);;			

comb 4 0;;
comb 4 5;;


(* pour une complexité de fou!!!**)


let rec comb2 n p =
match n,p with 
j,i when j<i ->0
|(_,0) -> 1
|_ -> (comb2 (n-1) (p-1)) * n/p;;


comb2 10 8;;

comb2 1000000 500000;;  (* stackoverflow mais on a déjà quitté le domaine 
entiers représentables...*)

(* on ne peut pas faire de la récu
 quand même faire de la récusrivité terminale  sauf à passer 
 en flottant... car n/p n'est pas entier*)


let combinaison n p=
	let rec aux nn pp res =
		match nn,pp with 
	j,i when j<i ->0.
	|(_,0) -> res
	|_ -> aux (nn-1) (pp-1)  (res*. float_of_int(nn)/.float_of_int(pp))
	in 
	aux n p 1.;;



(*******************************************************)
(*****************Exercice 15****************************)
(*******************************************************)





(*******************************************************)
(*****************Exercice 16****************************)
(*******************************************************)







(********************Algorithmes de tris****************************)


(* fonction qui envoie la case i sur i+1, i+1 sur i+2, et j-1 sur j et 
j sur i*)
let circule t i j=
    let sto = t.(j) in 
    for k=j downto  (i+1) do
    	t.(k)<-t.(k-1)
    done;
    t.(i)<-sto;;
    
let extab =[|12;1;3;6;9|];;
circule extab 1 3;;
extab;;



(*Le pb c'est qu'on va se retrouver avec un algo en O(n^2)*)


let affiche tab=
	for i=0 to Array.length tab -1 do
		print_int(tab.(i));
		print_char(';');
	done;
	print_newline();;
	
	
affiche [|1;5;6;8|];;
		



(* on utilise un curseur pour parcourir la moitié de gauche, un pour la partie droite
et un pour indiquer la case à remplir*)


let rec fusion t  a b bp c taux =

(* on commence par stocker les valeurs de la première plage*)
	for i= a to b do
		taux.(i)<- t.(i)
	done; 
	let curseur_g = ref a and curseur_d = ref bp  and curseur_res = ref a in 
	let cond = ref true in 
	while !cond do
		if taux.(!curseur_g) <= t.(!curseur_d) then 
			begin
			t.(!curseur_res) <- taux.(!curseur_g);
			curseur_g := !curseur_g +1;
			curseur_res := !curseur_res +1;
			if (!curseur_g = bp) || (!curseur_res = (c+1)) then cond:= false;
			end
		else
			begin 
			t.(!curseur_res) <- t.(!curseur_d);
			curseur_d := !curseur_d +1;
			curseur_res := !curseur_res +1;
			if (!curseur_d = c+1) || (!curseur_res = (c+1)) then cond:= false;
			end;
	done;;



(* si la plage à trier n'est pas de longueur nulle, on trie récusrivement chaque 
moitié puis on fusionne.  taux est simplement un tableau de stockage...*)



let rec trifusion t  deb fin taux =
if deb < fin  then  
begin 
	let milieu = (fin+deb)/2 in 
	trifusion t  deb milieu   taux;
	trifusion t (milieu+1) fin taux;
	fusion t deb milieu (milieu+1) fin taux;
end;;

let tri t =
let n = Array.length t in 
let tableau_stockage  = Array.make  n 0 in 
trifusion t 0 (n-1) tableau_stockage;;




let exple=  [|12;11;0;25;3;6;9;8;11;13;14;17|] in   
tri exple;
 affiche exple;;
 
 
print_char(',');;




