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

