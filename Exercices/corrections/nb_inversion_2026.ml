


let nb_inv_imp t = 
	let n = Array.length t in 
	let res = ref 0 in 
	for i = 0 to n - 1 do 
		for j = i + 1 to n - 1 do 
			if t.(i) > t.(j) then incr res;
		done
	done; 
	!res;;
	
	
nb_inv_imp [| 10;2;9;1;6;7|];;


(* version naive: 
nb i j acc renvoie acc + nb d'inversion (a,b) avec (i,j) <= (a,b)*)

let nb_inv_naif t =
	let n = Array.length t in 
	let rec nb i j acc =
		match n - i, n - j with 
		0, _ -> acc
		|_ , 0 -> nb (i + 1) (i + 2) acc
		|_,_ when t.(i) > t.(j) ->  nb i (j + 1)  (1 + acc) 
		|_,_  ->  nb i (j + 1)   acc
	in nb 0 1 0;;
	
	
nb_inv_naif [| 10;2;9;1;6;7|];;
	
	
let fusion t deb_g milieu fin_d = 
	let len_g, len_d = milieu - deb_g , fin_d - milieu in
	let copie_gauche = Array.sub t deb_g len_g in 
	let copie_droite = Array.sub t milieu len_d in 
	let rec remplir i_t i_g i_d acc_nb_inv =
	(* i_t indice de la case à remplir dans t,
	i_g indice de la première case dispo dans copie_gauche
	i_d indice de la première case dispo dans copie_gauche
	acc_nb_inv accumulateur du nombre d'inversions*)
		match len_g - i_g, len_d - i_d with 
		0, 0 -> acc_nb_inv
		|0, _ -> 		t.(i_t) <- copie_droite.(i_d); 
						remplir (i_t + 1) i_g (i_d + 1) acc_nb_inv
						
		|_,0 -> 		t.(i_t) <- copie_gauche.(i_g);
						 remplir (i_t + 1) (i_g + 1) (i_d) acc_nb_inv
		|lg_cour, ld_cour when copie_gauche.(i_g) <= copie_droite.(i_d) -> 
						t.(i_t) <- copie_gauche.(i_g);
						 remplir (i_t + 1) (i_g + 1) (i_d) acc_nb_inv
		|lg_cour, ld_cour (*when copie_gauche.(i_g) > copie_droite.(i_d)*) -> 
						t.(i_t) <- copie_droite.(i_d);
						 remplir (i_t + 1) (i_g) (i_d + 1) (lg_cour + acc_nb_inv)
	in remplir deb_g 0 0 0;;
	
	
	
let nombre_inversions t = 
	let t_bis = Array.copy t in 
	let rec calcule  debut fin  = 
	   if fin <= (debut + 1) then 0 else 
		let milieu = (debut + fin ) / 2  in
		(calcule debut milieu) +  (calcule milieu fin) + (fusion t_bis debut  milieu fin)
	in (t_bis, calcule 0 (Array.length t_bis) );; 
	
	

nombre_inversions [| 10;2;3;6|];;


(**** 
version copilotée***)



(* Fusionne les sous-tableaux triés t[deb_g .. milieu) et t[milieu .. fin_d),
   en place dans t, et renvoie le nombre d'inversions "croisées". *)
let fusion (t : int array) (deb_g : int) (milieu : int) (fin_d : int) : int =
  let len_g = milieu - deb_g
  and len_d = fin_d - milieu in
  let copie_gauche = Array.sub t deb_g len_g in
  let copie_droite = Array.sub t milieu len_d in

  let rec remplir i_t i_g i_d acc =
    (* i_t : index d’écriture dans t
       i_g : index de lecture dans copie_gauche
       i_d : index de lecture dans copie_droite
       acc : accumulateur d’inversions *)
    match (len_g - i_g, len_d - i_d) with
    | 0, 0 -> acc
    | 0, _ ->
        t.(i_t) <- copie_droite.(i_d);
        remplir (i_t + 1) i_g (i_d + 1) acc
    | _, 0 ->
        t.(i_t) <- copie_gauche.(i_g);
        remplir (i_t + 1) (i_g + 1) i_d acc
    | _, _  when  copie_gauche.(i_g) <= copie_droite.(i_d) ->  
                                begin 
                                  t.(i_t) <- copie_gauche.(i_g);
                                 remplir (i_t + 1) (i_g + 1) i_d acc )
                                 end
     |-,-  ->   begin  t.(i_t) <- copie_droite.(i_d) ;   let added = len_g - i_g in
          remplir (i_t + 1) i_g (i_d + 1) (acc + added) 
        
  in
  remplir deb_g 0 0 0
;;

(* Retourne (tableau trié, nombre d'inversions) *)
let nombre_inversions (t : int array) : int array * int =
  let t_bis = Array.copy t in
  let rec calcule debut fin =
    if fin <= debut + 1 then 0
    else
      let milieu = (debut + fin) / 2 in
      let g = calcule debut milieu in
      let d = calcule milieu fin in
      let c = fusion t_bis debut milieu fin in
      g + d + c
  in
  (t_bis, calcule 0 (Array.length t_bis))
;;

	


nombre_inversions [| 10;2;3;6|];;


(*********Notons que la version qui suit ne marche pas!!!!!***)
(**** Mais c'est fin!!!!!!!!!!!!***)
let nombre_inversions_faux (t : int array) : int array * int =
  let t_bis = Array.copy t in
  let rec calcule debut fin =
    if fin <= debut + 1 then 0
    else
      let milieu = (debut + fin) / 2 in
      (calcule debut milieu) + (calcule milieu fin) + (fusion t_bis debut milieu fin)     
 in (t_bis, calcule 0 (Array.length t_bis))
;;

(**** en effet: 
peut être évaluée dans un ordre non garanti par OCaml.
 Selon la version/compilateur, l’évaluation des opérandes de +
  n’est pas spécifiée de façon stricte (et des optimisations peuvent changer l’ordre).
   Du coup, il peut arriver que fusion s’exécute avant que les deux moitiés aient été triées,
    ce qui fausse le comptage.***)

nombre_inversions_faux [| 10;2;3;6|];;




