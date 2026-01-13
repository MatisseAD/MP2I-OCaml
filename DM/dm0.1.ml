(** Problème 1*)

(** 1 *)

let diviser l = 
  let l1  = ref [] in
  let l2 = ref [] in
  for i=0 to (List.length l) -1 do
    if i < (List.length l) / 2 then 
      l1 := List.nth l i :: !l1 (** Renvoie le n-ieme terme de la liste *)
  else
    l2 := List.nth l i :: !l2; (** Renvoie le n-ieme terme de la liste *)
  done;
  (!l1, !l2);;


(** 2 *)

let fusion l1 l2 = 
  let l_final = ref [] in
  
  let i1 = ref 0 in
  let i2 = ref 0 in

  while !i1 < List.length l1 && !i2 < List.length l2 do
    if List.nth l1 !i1 < List.nth l2 !i2 then (
      l_final := List.nth l1 !i1 :: !l_final;
      i1 := !i1 + 1
    ) else (
      l_final := List.nth l2 !i2 :: !l_final;
      i2 := !i2 + 1
    )
  done;

  if !i1 = (List.length l1) then
    for i = !i2 to ((List.length l2) - 1) do
      l_final := List.nth l2 i :: !l_final
    done
  else 
    for i = !i1 to (List.length l1) - 1 do
      l_final := List.nth l1 i :: !l_final
    done;

    !l_final;;
  
let rec tri liste = 
  match liste with
  | [] -> []
  | l ->  (diviser l)