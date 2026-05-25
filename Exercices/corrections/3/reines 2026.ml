(*************************************************************)
(***********Les reines***************************************)
(***********************************************************)


(* On va placer la reine i dans la colonne t.(i)*)

(* renvoie true si l'une des reines de case
(0,t.(0)), ...., (i, t.(i)) 
est en prise avec la case c*)


let rec en_prise t i c = 	
	let x_c, y_c = c in 
	match i with 
	k when k < 0 -> false
	|_  when (i = x_c) || (t.(i) = y_c)|| (x_c + y_c = i + t.(i))||
	(x_c -y_c = i -t.(i)) -> true 
	|_ -> en_prise t (i-1) c;;
	
	
(**** Backtracking pur, pour renvoyer toutes les solutions*)

(***************************************************)
(*************************Pas parfait, mais le plus facilement réemployable*****)
(************************************************************************)
(* la fonction solution_a_partir_de i, ajoute à res  toutes les solutions qui prolongent
(0,t.(0)),...., (i-1,t.(i-1)) *)

let reines_backtracking_imperatif n =
		let rec solution_a_partir_de i t res = 
		if i = n then 
			res := (Array.copy t)::(!res)
		else
			for k = 0 to n - 1 do 
				if not (en_prise t (i-1) (i,k)) then 
					begin
					t.(i) <- k;
					solution_a_partir_de (i + 1 ) t res
					end
			done
		in 
		let tab = Array.make n (-1) in 
		let toutes_les_sol = ref [] in 
		solution_a_partir_de 0  tab toutes_les_sol;
		!toutes_les_sol;;
		
		
reines_backtracking_imperatif 5;;


(** le même algo mais implémenté de manière plus fonctionnelle*)

(** solution à partir de i renvoie la liste obtenue 
 de toutes les solutions que l'on obtient 
en ajoutant à res toutes les solutions qui prolongent
 celles dans déjà mises jusqu'à la case 
d'indice i-1, par une case t.(i) d'indice >= k*)

let reines_backtracking_2 n =
		let rec solution_a_partir_de i k t res = 
		if i = n then 
			(Array.copy t)::(res)
		else
		if k = n then res 
		(* on épuisé tous les choix possibles, on ne peut prolonger t*)
		else 
			 begin if not (en_prise t (i-1) (i,k)) then 
					begin
					t.(i) <- k;
					let res_bis = solution_a_partir_de (i + 1) 0 t res 
					(* on ajoute les solutio ou t.(i) = k *)
					in 
					solution_a_partir_de i (k + 1) t res_bis
					(* on ajoute les solution ou t.(i) >= k*)
					end
					else
					solution_a_partir_de i (k + 1) t res
			end	
		in 
		let tab = Array.make n (-1) in 
		solution_a_partir_de 0  0 tab [];;
		
		

reines_backtracking_2 5;;



(************************************************************************************)
(***********Version fonctionnelle finalement plus simple peut-être à comprendre******)
(************************************************************************************)

let reines_backtracking_fonc n =
		let t = Array.make n (-1) in 
		let rec sol_apres i  = 
		if i = n then 
			[Array.copy t]
		else 
			let aux k = 
			   (* renvoie toutes les solutions qui prolongent t.(0),....,t.(i-1),k *)
				match en_prise t (i-1) (i,k) with 
				true -> [] (* y en pas *)
				|_ -> t.(i) <- k; sol_apres (i+1) 
			in List.init n (fun x -> x)   (* [0,...n-1]*)
			    |> (List.map aux)   (* on a la liste des listes des solutions avec les 
			    différentes valeurs possibles de t.(i) *)
			    	|> List.concat  (* on concatène le tout*)
		in 
	sol_apres 0;;	
	
	
	
	
reines_backtracking_fonc 5;;


(**********************************************************************************)
(**********************************************************************************)



		
		           
		           		


		
		


					
	
	
	
	
	