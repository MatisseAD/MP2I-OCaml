
(******************* SDA union-and-find ***********************)

type uf = 
{
	taille : int ; 
	contenu: int array; 
	hauteur: int array
};;


let init (n : int) : uf =
  let taille_uf = n in
  let tableau = Array.make n 0 in
  for i=0 to n-1 do
    tableau.(i) <- i
  done;
  let contenu_uf = tableau in
  let tableau_hauteur = Array.make n 0 in
  for i=0 to n-1 do
    tableau_hauteur.(i) <- 0;
  done;
  let hauteur_uf = tableau_hauteur in
  {taille = taille_uf; contenu = contenu_uf; hauteur = hauteur_uf};;


let find (iuf : uf) (v : int) =
  let c = ref v in
  let new_c = ref iuf.contenu.(v) in
  while !c <> !new_c do
    c := !new_c;
    new_c := iuf.contenu.(!new_c)
  done;
  !c
		
let rec find2 (iuf : uf) (v : int) =
  if iuf.contenu.(v) = v then
    v
  else
    find2 iuf iuf.contenu.(v)

		
let union iuf a b = 
  let representant_a, representant_b = find2 iuf a, find2 iuf b in
  let ha,hb = iuf.hauteur.(representant_a), iuf.hauteur.(representant_b) in
  if ha > hb then
    iuf.contenu.(representant_b) <- representant_a
  else if ha < hb then
    iuf.contenu.(representant_a) <- representant_b
  else 
    (* ha = hb *)
    iuf.contenu.(representant_b) <- representant_a;
    iuf.hauteur.(representant_a) <- iuf.hauteur.(representant_a) + 1;
    iuf.hauteur.(representant_b) <- -1;



let calcul_classe iuf : int list list =
  let n = iuf.taille in
  let representant = Array.make n (-1) in
  for i=0 to n-1 do
    representant.(i) <- find2 iuf i;
  done;
  let tab = Array.make n [] in
  for i=0 to n-1 do
    tab.(representant.(i)) <- i :: tab.(representant.(i));
  done;
  let rec aux l cpt =
    match cpt with
    | x when x=n -> l
    | p ->
      if tab.(p) <> [] then (
        aux (tab.(p) :: l) (p+1)
      )else
        aux l (p+1)
  in aux [] 0


(******************* Application : calcul des composantes connexes ***********************)

type graphe = 
{
	taille : int;
	voisins : int list array;

};;

let g_exp = 
{	taille = 7;
	voisins =
		[|
			[1;3];
			[0;2;4];
			[1;3];
			[0;2];
			[1];
			[6];
			[5];
		|]
};;

let composantes_connexe ungraphe =
  let n = ungraphe.taille in
  let contenu_uf = Array.make n (-1) in
  let t = ungraphe.voisins in
  contenu_uf.(0) <- 0;
  for i=0 to n-1 do
    List.iter (fun x ->
      if contenu_uf.(x) = -1 then
        contenu_uf.(x) <- i;
      ) ungraphe.voisins
      done;
  let iuf = {taille = n; contenu = contenu_uf; hauteur = [||]} in
  calcul_classe iuf

(******************* Le type pile ***********************)

type 'a pile = {mutable C: 'a list };;

let push_pile x P = 
	P.C <- x :: P.C;;
	
let pop_pile P = 
	let t = hd P.C 
	in 
	begin 
		P.C <- tl P.C; 
		t;
	end;;

let is_empty P =
  P.C = []

let new_pile () =
  let c = ref [] in
  {C = c}


(* Graphe exemple *)

let t_graphe_exp = [|[1;2];[2];[3;4];[4];[5];[];[4;5]|];;


(******************* parcours en profondeur d'un graphe avec une pile ***********************)

let liste_descendants_prof graphe sommet =
  let p = new_pile () in
  let n = graphe.taille in
  let deja_visite = Array.make n false in
  while not (is_empty) do
    
(* liste des descendants de sommet *)
(* int list array -> int -> int list *)

(* test *)
liste_descendants_prof t_graphe_exp 0;;


(******************* structure de file  avec deux piles ***********************)

type 'a file = {mutable G: ('a list); mutable D: ('a list)};;

let push_file x F =
	F.D <- x :: F.D;;
	
let pop_file F =
	if F.G <> []
	then
		let t = hd F.G in
		begin
			F.G <- tl F.G;
			t;
		end
	else
	begin
		while F.D <> [] do
			let t = hd F.D in
			begin
				F.G <- t :: F.G;
				F.D <- tl F.D;
			end;
		done;
		let t = hd F.G in
		begin
			F.G <- tl F.G;
			t;
		end;
	end;;
	
(******************* parcours en largeur d'un graphe avec une file ***********************)
let liste_descendants_larg_file graphe sommet = 
(* liste des descendants de sommet *)
(* int list array -> int -> int list *)


(* test *)
liste_descendants_larg_file t_graphe_exp 0;;



(******************* calcul des distances � un sommet de d�part (en nombre d'ar�tes) ***********************)
let liste_distance_larg graphe sommet = 
(* calcul des distances � un sommet de d�part : sommet *)
(* int list array -> int -> (int*int) list *)

(* test *)
liste_distance_larg t_graphe_exp 0;;



(******************* file de priorit� avec un tas max ***********************)

type file_prior = {mutable taille:int ; contenu: (int*int) array};;
(* un �l�ment de la file est un couple (valeur,priorit�) *)

let echange t i j =
 let s = t.(i) in begin t.(i) <- t.(j); t.(j) <- s end;;


let push_prior file e =
(* insertion dans un tas-max *)
(* file_prior -> (int*int) -> unit *)
	
let pop_prior file =
(* extraction d'un �l�ment de priorit� maximale un tas-max *)
(* file_prior -> int -> unit *)

(******************* tri par tas ***********************)
let tri t =
(* tableau t tri� *)
(* int array -> unit *)

(******************* Et pour finir : Dijkstra ! ***********************)







	

		

		
		
		
		
		
		
		
		


	
