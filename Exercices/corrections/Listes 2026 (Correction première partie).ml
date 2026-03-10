

(*******************************************************************)
(********************Exercice 1 ************************************)
(*******************************************************************)




let rec suite n = 
	let rec transforme liste = 
	(* prend une liste et transforme les 0 en 10 les 1 en 01*)
	match liste with 
	[] -> []
	|t::q when t = 0 -> 1::0::(transforme q)
	|t::q when t = 1 -> 0::1::(transforme q)
	|t::q -> failwith("ya un souci")
	in 
	match n with 
	0 -> [0]
	|_ -> transforme (suite (n-1));;
	
	

(* On peut aussi utiliser un fold_left pour définir la fonction de transformation 
des listes*)

let transforme_bis liste = 
	let f li a = 
		if a = 0 then li@[1;0] else
			li@[0;1]
	in List.fold_left f [] liste;;
	
transforme_bis [0;1;0];;
	


suite 3;;


(* Avec un List.map qui permet d'avoir des listes de listes, puis un applatissage*)


let next l = 
	List.map (fun x -> if x= 0 then [0;1] else [1;0]) l |> List.flatten;;
	
let rec suite n = 
	match n with 
	0 -> [0]
	|_ -> next (suite (n-1)) ;;

(*******************************************************************)
(********************Exercice 2 ************************************)
(*******************************************************************)



(*** coupure d'une liste après n éléments***)



let rec coupure liste n=
    match liste with 
    [] -> ([],[])
    |_ when n = 0 -> ([], liste)
    |t::q -> let l1,l2 = coupure q (n-1) in 
                  (t::l1, l2);;


coupure [12;32;15;69;10;1] 8;;



(*** entrelacement de deux listes***)


let rec shuffle l1 l2=
	match l1, l2 with 
	[], _ -> l2
	|_, [] -> l1
	|t1::q1,t2::q2-> t1::t2::(shuffle q1 q2);;
	
	
shuffle [2;4;6;8;10] [1;3;5;7;9;11;13];;





(*******************************************************************)
(********************Exercice 3 ************************************)
(*******************************************************************)




(******* sous-listes*****)

(* on regarde si l1 est une sous-liste de l2*)

let  rec estsousliste l1 l2=
	match l1, l2 with 
	[], _ -> true
	|_, [] -> false
	|t1::q1, t2::q2 when t1=t2 -> estsousliste q1 q2
	|t1::q1, t2::q2-> estsousliste l1 q2;;


estsousliste [2;5;6]  [1;2;3;4;5;7;8];;


let rec listedessouslistes l=
	match l with 
 [] -> [[]]
 |t::q -> let m = listedessouslistes q in 
 m @ (List.map  ( fun l -> t::l) m);;
 
listedessouslistes [14;12;15;2];;




(*******************************************************************)
(********************Exercice 4 ************************************)
(*******************************************************************)


(*** code de Gray***)


(* On met les mots de l précédés de 0 puis ceux de List.rev l précédés de 1*)


(* Ce serait plus logique d'accepter aussi le List.rev*)

let rec gray n=
	match n with 
	0 -> [[0]]
	|_ -> let prec = gray (n-1) in 
      (List.map (fun l -> 0::l) prec ) @
        (List.map (fun l -> 1::l) (List.rev prec));;
      

gray 2;;
gray 3;;





(*******************************************************************)
(********************Exercice 5 ************************************)
(*******************************************************************)



let taillemax = 10;;
type 'a liste = {sommet:  int; contenu: 'a array};;
(* on convient que le sommet est la dernière case occupée*)

(* x est un objet de valeur arbitraire, seul sont type importe*)
let vide x = {sommet = -1; contenu = Array.make taillemax x};;


let ex1 = {sommet = 7; contenu =[|14;12;12;13;-1;2;8;1;-1;-1|]};;
let ex2 = {sommet = 4; contenu = [|0;1;2;3;4;-1;-1;-1;-1;-1;-1|]};; 

let estpleine l=
	l.sommet = (taillemax - 1);;
	
let est_vide l=
	l.sommet = (-1);;
	
let ajouter x l= (* renvoie une nouvelle liste, contenant un élément de plus*)
	if estpleine l then 
		failwith("ajout impossible")
	else
				begin 
			let res = {sommet = l.sommet +1;
						 contenu = Array.make taillemax (l.contenu).(l.sommet)}; 
				in begin 
				for k = 0 to l.sommet do 
					(res.contenu).(k)<- (l.contenu).(k);
				done;
				(res.contenu).(res.sommet) <- x;
			res;
			end;
			end;;

let ex1 = {sommet = 7; contenu =[|14;12;12;13;-1;2;8;1;-1;-1|]};;
let  ex2 = ajouter 10 ex1;;
let ex3 = ajouter 10 ex2;;


let tete l=
	if est_vide l then 
		failwith("il n'y a pas de tête")
	else
		(l.contenu).(l.sommet);;


let queue l=
	if est_vide l then
		failwith("il n'y a pas de queue")
	else
		begin 
		let queue = vide (l.contenu).(l.sommet) in 
		begin 
		queue.sommet <- ((l.sommet) -1);
		for k = 0 to queue.sommet do
			(queue.contenu).(k) <- (l.contenu).(k); 
		done;
		queue;
		end;
		end;;
			
	
(* on peut alors récrire les fonctions usuelles, sans se poser de question 
ni mettre les mains dans le cambouis*)

		
let maxi x y=
 if x>y then x else y;;		
			
let rec maxi_aux  l nb=
	match est_vide l with 
	true -> nb;
	|false -> maxi_aux (queue l) (maxi (tete l) nb);;
	
		
let maximum l=
	if est_vide l then failwith("y a personne")
	else maxi_aux l (tete l);;
	


(*******************************************************************)
(********************Exercice 6 ************************************)
(*******************************************************************)




type  intliste = Vide | Couple of int*intliste;;


let l1 = Couple(4, Couple(3, Couple(2, Vide)));;

let l2 = Couple (6,  Couple(4, Couple(3, Couple(5, Vide))));;


let est_vide l=
match l with 
Vide -> true; 
|_-> false;;

let ajouter x l=
	Couple(x, l);;
	
let tete l=
	if est_vide l then failwith(" pas de tête")
	else
		begin 
		let Couple(t, q) = l in 
		t;
		end;;
		
let queue l=
	match l with 
	Vide -> failwith("pas de queue")
	|Couple (t, q) -> q;;
	
queue l2;;
tete l2;;



(*******************************************************************)
(********************Exercice 7 ************************************)
(*******************************************************************)


(*****************Opérations sur les polynômes***********************)


type listep = int*int list;;

let rec aux  tab i res   = 
	match  i with 
	-1 -> res
	|_ -> let new_res = (i, tab.(i))::res in 
	aux tab (i-1) new_res;;
	
let tableau_vers_liste  t = 
	let n = Array.length t  in 
		aux t (n-1) [];;




(**** vieille version***)


let tableau_vers_liste tab=
let rec  tab_to_list t prems = 
	match prems with 
	-1 -> [];
	|_  when t.(prems) = 0 -> tab_to_list t (prems-1);
	|_->	 let monome = (prems, t.(prems)) in 
		monome::(tab_to_list t (prems-1));
in 
	tab_to_list tab (Array.length tab-1);;
		
		
tableau_vers_liste [|14;0;0;0;1;0;0;9|];;


let listep1 =  tableau_vers_liste [|2;1;3;1;-1|];;
let listep2 = tableau_vers_liste [|-3;1;2|];;


let rec add lp lq=
	match lp, lq with 
	[],_-> lq
	|_,[]-> lp
	|(degp, coeffp)::lpp, (degq,coeffq)::lqq when degp>degq->
	        (degp,coeffp)::(add lpp lq)
	|(degp, coeffp)::lpp, (degq,coeffq)::lqq when degp<degq->
	        (degq,coeffq)::(add lp lqq)
	 |(degp, coeffp)::lpp, (degq,coeffq)::lqq when degp=degq->
	         (degp, coeffp+coeffq)::(add lpp lqq);;
	         
add listep1 listep2;;


let rec prod scal lp=
	match lp with 
	[] -> []
	|(deg, coeff)::lpp -> (deg, scal*coeff)::(prod scal lpp);;
	
	
let rec prod_monome lp n  alpha =(* multiplie par alpha X^n*)
	match lp with
	[]-> []
	|(deg, coeff)::lpp -> (deg+n, alpha*coeff)::(prod_monome lpp n alpha);;
	
let rec prod_poly lp lq=
	match lp with 
	[]-> []
	|(degp, coeffp)::lpp -> add (prod_poly lpp lq) (prod_monome lq degp coeffp);;


let listep3 =  tableau_vers_liste [|1;2;1|];;
let listep4 = tableau_vers_liste [|1;1|];;


prod_poly listep3 listep4;;


let rec derive lp=
	match lp with 
	[] -> []
	|(deg, coeff)::lpp when deg = 0 ->derive lpp
	|(deg,coeff)::lpp -> (deg-1, deg*coeff)::(derive lpp);;


derive listep3;;

let degre lp=
match lp with 
	[] -> -1
	|(deg, coeff)::lpp ->deg;;
	
	
	


(* division euclidienne, supposons q de coeff dominant 1*)
let rec division listep listeq=
	match (degre listep)-(degre listeq) with 
	d when d<0 -> (0, listep)
	|d  -> let (degp, coeffp) = List.hd listep in 
		division  (add listep (prod_monome listeq degp (-coeffp))) listeq;;


division listep3 listep4;;


(*******************************************************************)
(********************Exercice 8 ************************************)
(*******************************************************************)



type operation = Plus | Fois | Moins;;

type algebre = Op of operation | Nombre of int;;


let essai = [Op Plus; Op Fois;Nombre 3;Nombre 2; Op Fois; Op Fois; Nombre 4;
Nombre 5; Op Moins; Nombre 1];; 



(* la fonction suivante renvoie le resultat de la première opération et
le reste de la liste*)

let rec lecture liste =
	match liste with 
	[] -> failwith("liste mal construite")
	|(Nombre k)::q -> (k, q)
	|(Op Moins)::q-> 
	let res1, liste1 = lecture q in 
		(- res1), liste1
	|(Op Plus)::q -> 
	     let res1, liste1  = lecture q in 
	     let res2, liste2 = lecture liste1 in 
	     res1 + res2, liste2
	 |(Op Fois)::q -> 
	     let res1, liste1  = lecture q in 
	     let res2, liste2 = lecture liste1 in 
	     res1 * res2, liste2;;
	     
	     
let evalue liste = 
let res, _ = lecture liste in res;;

evalue essai;;



(*******************************************************************)
(********************Exercice 9 ************************************)
(*******************************************************************)




(*version 2022*)



(*renvoie la liste triée par ordre décroissant de tous les nb de fibonacci <= x*)
let listefibo x =
let rec aux a b x  res = 
 (* ajoute à res tous les nb de 
 la suite de premier terme a et de terme suivant b tq u_n+2 = u_n +u_(n+1)
 inférieurs à x*)
 if a > x then res 
 else aux b (a+b) x (a::res)
 in aux 0 1 x [];;



listefibo 125;;


(*décomposition en base de Fibonacci*)
let decompose x = 
 let rec aux fibiens x res=
 	match x, fibiens with 
 	0, _ -> res 
 	|_, t::q when  t>x  -> aux q x res
 	|_, t::q -> aux q (x-t) (t::res)
 	|_ -> failwith( "c'est pas normal")
 in aux (listefibo x) x [];; 
 
 decompose 131;;

(*recomposition*)
let compose liste=
  List.fold_left (+) 0 liste;; 
  
  
compose [8;34;89];;





(**** version 2023**********)

******************************décomposition en base Fibonacci ***************)


(*renvoie la liste triée par ordre décroissant de tous les nb de fibonacci <= x*)
let listefibo x =
let rec aux a b x  res = 
 (* ajoute à res tous les nb de la suite de premier terme a et de terme
  suivant b tq u_n+2 = u_n +u_(n+1)
 inférieurs à x*)
 if a > x then res 
 else aux b (a+b) x (a::res)
 in aux 0 1 x [];;



listefibo 125;;


(*décomposition en base de Fibonacci*)
let decompose x = 
 let rec aux fibiens x res=
 	match x, fibiens with 
 	0, _ -> res 
 	|_, t::q when  t>x  -> aux q x res
 	|_, t::q -> aux q (x-t) (t::res)
 	|_ -> failwith( "c'est pas normal")
 in aux (listefibo x) x [];; 
 
 decompose 131;;

(*recomposition*)
let compose liste=
  List.fold_left (+) 0 liste;; 
  
  
compose [8;34;89];;

(*******************************************************************)
(********************Exercice 10 ************************************)
(*******************************************************************)





(*******************************************************************)
(********************Exercice 11 ************************************)
(*******************************************************************)


(***********Tri par insertion************)

let rec insere x l = 
	match l with 
	[] -> [x]
	|t::q when x <= t -> x::l
	|t::q -> t::(insere x q);;
	
	
let rec tri_insertion l =
	match l with 
	[] -> []
	|[t] -> [t]
	|t::q -> insere t (tri_insertion q);;
	
	
tri_insertion [12;13;1;5;8;9];;
	
	
	

let rec separation l fait1 fait2=
	match l with 
	[] -> fait1, fait2
	|[a] ->  (a::fait1), fait2
	|a::b::q -> separation q (a::fait1) (b::fait2);;


let diviser l=
	separation l [] [];;


diviser [12;13;2;5;6;7];;	
	
	
	
(* mais ce n'est pas terminal... est-ce faisable? oui et on s'en fout et surtout
oui mais l'on va perdre l'intérêt*)
let rec fusion2 l1 l2  =
	match l1,l2 with 
	[],_-> l2 
	|_,[] -> l1 
	|a::q1,b::q2 when a<b ->  a::(fusion2 q1 l2 )
	|a::q1, b::q2 -> b::(fusion2 l1 q2);;	
	
let rec trifusion l=
	match l with 
	[] -> [];
	|[a] -> [a];
	|_-> let lg, ld = diviser l in 
	begin fusion2 (trifusion lg) (trifusion ld); end;;
		
		
trifusion [1;5;21;13;69;52;74];;



(**** le quicksort pour les listes***)

let rec separe liste pivot avant apres =
	(* rajoute a avant les elements de listes inf au pivot, a apres les autres*)
	match liste with 
	[] -> (avant, apres)
	|t::q  when t <= pivot -> separe q pivot (t::avant) apres
	|t::q -> separe q  pivot avant (t::apres);;
	
	
separe [3;9;8;10;11;12] 8 [] [];;
	
	
let rec quicksort liste = 
	match liste with 
	[] -> []
	|[t] -> [t]
	|t::q -> let avt, apr = separe q t [] [] in 
		(quicksort avt)@[t]@(quicksort apr);;
		
		
	
quicksort [12;1;5;7;8;9;19;32];;

(*******************************************************************)
(********************Exercice 12 ************************************)
(*******************************************************************)




(************décomposition en sous-séquences monotones maximales***)

(* aux ajoute à résultat les sous-séquences monotones maximales de la liste 
à traiter en ajoutant si possible en tête de la première séquence ajoutée 
la séquence encours dont la monotonie est codée par monotonie et la dernière case
vaut prédécesseur*)







let decompose3 liste=
(* décomposition en sous-séquences monotones maximales*)
let rec aux monotonie  encours atraiter resultat=
	match atraiter, encours with 
	[], [] -> List.rev resultat
	|[], _  ->   List.rev ((List.rev encours)::resultat)
	|t::q, [] ->  aux 0 [t] q resultat
	|t::q, a::qe when monotonie = 0 && t = a -> aux 0 (t::encours) q resultat
	|t::q, a::qe when monotonie = 0 && t < a -> aux (-1) (t::encours) q resultat
	|t::q, a::qe when monotonie = 0 (* and t > a *) -> aux (1) (t::encours) q resultat 
	|t::q, a::qe when (t - a) * monotonie >= 0 -> 
	   (* on poursuit la séance en cours*)
	   aux monotonie (t::encours) q resultat
	|t::q, a::qe -> (* on commence une nouvelle séquence*)
		aux 0 [t]  q ((List.rev encours)::resultat)
in aux 0 [] liste [];;
	

decompose3 [1;5;8;8;2;3;4;3;2;2;1;0;1;5;6];;





(*******************************************************************)
(********************Exercice 13 ************************************)
(*******************************************************************)




(*********************détermination de la tranche de somme maximale************)


(** On calcule deux choses: valeur de  la tranche initiale maximale et 
la tranche maximale***)

(*** on suppose que l'on a une liste d'entiers***)
let  tranche_max liste=
	let rec aux l=
		match l with 
		[]-> failwith("pas défini")
		|[r]-> r,r
		|t::q -> let ti_max,t_max= aux q in 
			let new_ti_max = max t (t + ti_max) in 
			   new_ti_max, (max new_ti_max t_max)
	in snd (aux liste);;


(**la complexité est bien linéaire***)

tranche_max [1;3;5;-1;10; 1; -3;-2; -6;-6];;	
tranche_max [5;-1];;
tranche_max [5;-1];;		
tranche_max [-2;1;3;5;-1;10; 1; -3;-2; -6;-6];;	



let tranche_max_elements liste=
	let rec aux l =
	match l with 
	[]-> failwith("pas defini")
	|[r]-> r,r,[r],[r]
	|t::q-> let ti_max, t_max, li_max,l_max = aux q in 
	      if t > t + ti_max then 
	      begin let new_ti_max, new_li_max = t, [t] in 
	            if new_ti_max > t_max then new_ti_max, new_ti_max, new_li_max, new_li_max
	                                  else new_ti_max, t_max, new_li_max, l_max
	      end
	                       else
	      begin 
	      let new_ti_max, new_li_max = (t + ti_max), (t::li_max) in 
	      		if new_ti_max > t_max then new_ti_max, new_ti_max, new_li_max, new_li_max
	                                  else new_ti_max, t_max, new_li_max, l_max
	      end
	 in let ti,tm,li,lm = aux liste in (tm,lm);;
	 
	 
tranche_max_elements [-2;1;3;5;-1;10; 1; -3;-2; -6;-6];;	


		

(*******************************************************************)
(********************Exercice 14 ************************************)
(*******************************************************************)





(******************Jeu de solitaire************)


let rec vider n =
	match n with 
	1-> [1]
	|2-> [2;1]
	|_ -> (vider (n-2))@(n::(remplir (n-2))) @ (vider (n-1))
	and 
 remplir n =
	match n with 
	1 -> [1]
	|2 -> [1;2]
	|_ -> (remplir (n-1)) @ (vider (n-2)) @ (n::(remplir (n-2)));;
	
	
(* mais on peut ensuite simplifier en notant que vider n = List.rev (remplir n) *)


vider 4;;
(* renvoie [2; 1; 4; 1; 2; 1; 3; 1; 2; 1]*)
vider 5;;
(* renvoie [1; 3; 1; 2; 1; 5; 1; 2; 1; 3; 1; 2; 1; 4; 1; 2; 1; 3; 1; 2; 1]*)


(* cela nous donne*)


let  rec remplir n =
	match n with 
	1 -> [1]
	|2 -> [1;2]
	|_ -> (remplir (n-1)) @ (List.rev (remplir (n-2))) @ (n::(remplir (n-2)));;


remplir 4;;
