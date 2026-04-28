
type sommet = int;;
type arc = int*int;;
type graphe = {n:int; liste_adj : int list array};;


(*
___________________________________________________________________________
___________________Exemple-------------------------------------------------
*)

let ex1 = {n = 9; liste_adj = [|(*v0*) [1;3]; (*v1*)[2]; (*v2*)[0];
(*v3*)[4]; (*v4*)[5];  (*v5*)[6;8]; (*v6*) [7];
(*v7*)[5]; (*v8*)[0]; |]};;



(*--------------------------------------------------------------------------
 On transforme un graphe orienté en un graphe non orienté*)
(* On risque de fabriquer des doublons mais c'est pas grave pour la suite*)

let desoriente (g: graphe): graphe = 
	let new_g = {n = g.n; liste_adj = Array.copy g.liste_adj} in 
	for i = 0 to g.n - 1 do
		List.iter
		 (fun som -> new_g.liste_adj.(som) <- i:: new_g.liste_adj.(som))
		  g.liste_adj.(i);
	done; 
	new_g;;
	
	


let est_connexe (g:graphe):bool=
	let n_g = g.n in 
	let deja_vu = Array.make n_g false
	in 
	let rec visite a_voir =
	 	match a_voir with 
	 	[] -> ()
	 	|som::q when  deja_vu.(som) -> visite q
	 	|som::q -> deja_vu.(som)<-true; visite (((g.liste_adj.(som)))@q)
	in 
	visite [0];
	let k = ref 0 and 
	res = ref true in  
	while !res && !k<n_g do
		if not deja_vu.(!k) then 
		 res := false
		else incr k;
	done; 
	!res;;
		 
		 
let est_equilibre (g:graphe):bool=
	let n_g = g.n in 
	let delta  = Array.make n_g 0 (*delta sortant - entrant*) in 
	let rec diminue liste  =
		match liste with 
		[] -> ()
		|t::q ->  delta.(t) <- delta.(t)-1; diminue q;
	in 
	for k = 0 to n_g-1 do
		delta.(k) <- delta.(k) + List.length ((g.liste_adj).(k));
		diminue ((g.liste_adj).(k));
	done; 
	delta = Array.make n_g 0;;
	
	

	
	
	
est_equilibre ex1;;

let admet_circuit_eulerien g = (est_equilibre (desoriente g)) && (est_connexe g);;
	 
	 
let ex1 = {n = 9; liste_adj = [|(*v0*) [1;3]; (*v1*)[2]; (*v2*)[0];
(*v3*)[4]; (*v4*)[5];  (*v5*)[6;8]; (*v6*) [7];
(*v7*)[5]; (*v8*)[0]; |]};;


desoriente ex1;;

est_connexe ex1;;



(*fait un circuit partant de v et élimine les arcs empruntés*)
(* la fonction continue, prolonge le chemin way jusqu'à trouver le sommet 
v, et agit par effet de bord sur le graphe, en éliminant les arêtes empruntés*)

(* dans way si way = ....,a_k, a_(k+1), a_(k+2),..., alors 
a_(k+1)->a_k est un arc, a_(k+2)->a_(k+1) est un arc, mais continue renvoie le c
chemin à l'endroit***)


let fait_bb_circuit (v:sommet) (g:graphe)=
	let rec continue way v =
	match way with 
	[] -> failwith(" ce cas ne doit pas arriver")
	|s::q when s = v -> List.rev way
	|s::q -> let s1::q1 = (g.liste_adj).(s) in 
		(g.liste_adj).(s) <- q1; continue (s1::way) v
	in 
	match (g.liste_adj).(v) with 
	 [] -> failwith("pas eulerien")
	  | s2::q2-> begin 	(g.liste_adj).(v) <- q2; continue [s2;v] v; end;;
	  
	
(*test*)  
fait_bb_circuit 0 ex1;;

ex1;;


(*complète un chemin way en un circuit eulérien, et agit 
par effet de bord sur le graphe en supprimant les sommets existants*)


let rec termine (way: sommet list) (g:graphe)=
	match way with 
	[] -> []
	|s::q -> begin
				match g.liste_adj.(s)  with 
				 [] -> s::(termine q g)
				|a::b ->   let baby = (fait_bb_circuit s g)
				 in termine (baby@q) g
				 end ;;
				 
				 
				 

let circuit_eulerien g = 
	termine [0] g;;

circuit_eulerien ex2;;


let ex2  = {n = 5; liste_adj = [| [1]; [2;4]; [3];[1]; [0] |]};;


termine [0] ex2 ;;

termine [0] ex1;;


let ex1 = {n = 9; liste_adj = [|(*v0*) [1;3]; (*v1*)[2]; (*v2*)[0];
(*v3*)[4]; (*v4*)[5];  (*v5*)[6;8]; (*v6*) [7];
(*v7*)[5]; (*v8*)[0]; |]};;

termine [0] ex1;; 
