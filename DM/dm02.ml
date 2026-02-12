  (**

  Début de l'en tête

  *)

  let base = 4;; (**Pour tester les fonctions*)
  type nat = int list;;

let drop_zero (n : nat) : nat =
  let rec drop_head_zeros = function
    | 0 :: t -> drop_head_zeros t
    | l -> l
  in
  List.rev (drop_head_zeros (List.rev n))
;;

  (**

  Fin de l'en tête

  *)

  (** Q1 **)

  let cons_nat (c : int) (n : nat) : nat =
    c :: n;;

  (** Q2 **)

  let retain (t1 :int) (t2 : int) : int*int =
    let retenue = ref 0 in
    let somme = ref (t1+t2) in
    while !somme >= base do
      somme := !somme - base;
      retenue := !retenue + 1;
    done;
    (!somme, !retenue);;

  let rec add_x (x : int) (n : nat) : nat =
    drop_zero (
    match n with
    | [] ->
        if x = 0 then [] else [x]  
    | h :: t ->
        let s = h + x in
        (s mod base) :: add_x (s / base) t
    )
  ;;

  let rec add_nat (n1 : nat) (n2 : nat) : nat =
    drop_zero (
      match n1,n2 with
      | [],_ -> n2
      | _,[] -> n1
      | t1::q1, t2::q2 when t1+t2 < base -> (t1 + t2) :: add_nat q1 q2
      | t1::q1, t2::q2 -> 
        let p1, p2 = retain t1 t2 in
        p1 :: add_nat (add_x p2 q1) q2
    )
  ;;

  (** Q3 **)

  let cmp_nat (n1 : nat) (n2 : nat) : int =
    let balance = ref 0 in
    let rec aux n1 n2 = 
      match n1, n2 with
      | [], [] -> balance := 0
      | [],_ -> balance := -1
      | _,[] -> balance := 1
      | t1::q1, t2::q2 -> if t1 > t2 then balance := 1 else if t2 > t1 then balance := -1 else aux q1 q2
    in
    aux (List.rev n1) (List.rev n2); (** Car le lsd est le 0-ième élément de la liste, on veut le mettre en dernière position *)
    !balance;;

  (** Q4 **)

  (** Supposons que n1 >= n2*)

  let sub_retain (t1 : int) (t2 : int) : int * int =
    let diff = t1 - t2 in
    if diff >= 0 then (diff, 0) else (diff + base, 1)
  ;;

  let rec sous_nat (n1 : nat) (n2 : nat) : nat = 
    drop_zero (
    match n1, n2 with
    | _, [] -> n1 (** Le cas [],_ est impossible car on suppose que n1 >= n2*)
    | t1::q1, t2::q2 when t1 - t2 >= 0 -> (t1-t2) :: sous_nat q1 q2
    | t1::q1, t2::q2 ->  let p1, p2 = sub_retain t1 t2 in
    p1 :: sous_nat q1 (add_x p2 q2) 
    )
  ;;

  (** Q5 **)

  let div2_nat (n : nat) : nat * int =
    let msd = List.rev n in  (* msd en premier *)
    let rec aux r = function
      | [] -> ([], r)
      | h :: t ->
          let v = r * base + h in
          let digit = v / 2 in
          let r' = v mod 2 in
          let q, last_r = aux r' t in
          (digit :: q, last_r)
    in
    let q_msd, r = aux 0 msd in
    (drop_zero  (List.rev q_msd), r)
  ;;


  type z = {signe: int; nat: nat};;

  (** Q6 **)

  let neg_z (z_val : z) : z =
    {signe = z_val.signe * (-1); nat = z_val.nat};;

  (** Q7 **)

let add_z (z1 : z) (z2 : z) : z =
  let res =
    if z1.signe = 0 then z2
    else if z2.signe = 0 then z1
    else if z1.signe = z2.signe then
      {signe = z1.signe; nat = add_nat z1.nat z2.nat}
    else
      let c = cmp_nat z1.nat z2.nat in
      if c > 0 then
        {signe = z1.signe; nat = sous_nat z1.nat z2.nat}
      else if c < 0 then
        {signe = z2.signe; nat = sous_nat z2.nat z1.nat}
      else
        {signe = 0; nat = []}
  in
  { res with nat = drop_zero res.nat }

let sous_z (z1 : z) (z2 : z) =
  add_z z1 (neg_z z2) ;;

  (** Q8 **)

  let rec mul_puiss2_z (p : int) (z_val : z) : z =
    if p = 0 then
      z_val
    else
      mul_puiss2_z (p-1) (add_z z_val z_val)
    ;;

let decomp_puiss2_z (z_val : z) : z * int =
  let p = ref 0 in
  let value = ref z_val.nat in
  while snd (div2_nat !value) = 0 do
    let v, _ = div2_nat !value in
    value := v;
    p := !p + 1;
  done;
  ({signe = z_val.signe; nat = !value}, !p)
;;


  (** II **)

  (** Q10 **)

  type dya = { m : z ; e : int};;

  let div2_dya (d : dya) : dya =
    { m = d.m; e = d.e -1};;

  (** Q11 **)

  let add_dya (d1 : dya) (d2 : dya) : dya =
    if d1.e > d2.e then
      {m = add_z (mul_puiss2_z (d1.e - d2.e) d1.m) d2.m; e = d2.e }
    else
      {m = add_z (mul_puiss2_z (d2.e - d1.e) d2.m) d1.m; e = d1.e}


  (** Q12 **)

  let sous_dya (d1 : dya) (d2 :dya) : dya = 
    if d1.e > d2.e then 
      {m = sous_z (mul_puiss2_z (d1.e - d2.e) d1.m) d2.m; e = d2.e }
    else
      {m = sous_z (mul_puiss2_z (d2.e - d1.e) d2.m) d1.m; e = d1.e}

  (** III **)

  type ldb = {
    lg : int ; g : dya list ;
    ld : int ; d : dya list ;
  }

  (** Q13 **)

  let ldb_est_vide (db : ldb) : bool =
    if db.lg = 0 && db.ld = 0 then true else false;;

  (** Q14 **)

  let premier_g (db : ldb) : dya =
    if db.lg <> 0 then
      match db.g with
      | x :: _ -> x
    else
      let rec aux lst =
        match lst with
        | [w] -> w
        | x :: t -> aux t
        | [] -> failwith "Impossible d'atteindre ce cas" (** Cas ou liste vide*)
      in
      aux db.d;;


  (** Q15 **)

  let inverse_ldb (db : ldb) : ldb = 
    {lg = List.length db.d; g = db.d; ld = db.lg; d = List.rev db.g}

  (** Q16 **)

  (** On suppose c >= 2, où c est une constante entière*)

let c = 3;; (** Pour avoir une valeur de test *)
let invariant_ldb (db : ldb) : ldb =
  if db.lg <= c * db.ld + 1 && db.ld <= c * db.lg + 1 then
    db

  (* Trop d'éléments à gauche : on en transfère une partie de g vers d *)
  else if db.lg > c * db.ld + 1 then
    let nb = db.lg / 2 in
    let rec take k l =
      if k = 0 then [] else
      match l with
      | [] -> []
      | x::t -> x :: take (k-1) t
    in
    let rec drop k l =
      if k = 0 then l else
      match l with
      | [] -> []
      | _::t -> drop (k-1) t
    in
    let removed = take nb db.g in          (* éléments retirés du début de g *)
    let new_g   = drop nb db.g in
    let new_d   = (List.rev removed) @ db.d in  (* transfert dans d : d est inversée *)
    { lg = List.length new_g; g = new_g;
      ld = List.length new_d; d = new_d }

  (* Trop d'éléments à droite : on en transfère une partie de d vers g *)
  else
    let nb = db.ld / 2 in
    let rec take k l =
      if k = 0 then [] else
      match l with
      | [] -> []
      | x::t -> x :: take (k-1) t
    in
    let rec drop k l =
      if k = 0 then l else
      match l with
      | [] -> []
      | _::t -> drop (k-1) t
    in
    let removed = take nb db.d in          (* éléments retirés du début de d (donc extrémité droite) *)
    let new_d   = drop nb db.d in
    let new_g   = db.g @ (List.rev removed) in  (* transfert dans g : on remet en bon ordre *)
    { lg = List.length new_g; g = new_g;
      ld = List.length new_d; d = new_d }
;;


let ajoute_g (d : dya) (db : ldb) : ldb =
    {lg = db.lg + 1; g = d :: db.g; ld = db.ld; d = db.d};;

let enleve_g (db : ldb) : ldb =
    match db.g with
    | [x] -> {lg = 0; g = []; ld = db.ld; d = db.d}
    | x :: t -> {lg = db.lg - 1; g = t; ld = db.ld; d = db.d}
    | [] -> 
      let rec aux lst =
        match lst with
        | [w] -> []
        | x :: t ->  x :: aux t
        | [] -> failwith "Impossible d'atteindre ce cas" (** Cas ou liste vide*)
      in 
      {lg = 0; g=[]; ld = db.ld - 1; d = aux db.d}

  (** Q19 **)

(**

Soit C(n), la complexité de la fonction ajoute_g.

A chaque ajoute_g, on vérifie si l'invariant est vrai, sinon on le rend vrai.
Supposons que l'invariant est vrai, alors on a un coût constant.
Sinon l'invariant est faux. Alors la fonction invariant_ldb est éxécuter pour rétablie l'invariant.
Comme pour chaque condition on parcourt au plus la moitié de la liste, on a une complexité linéaire par rapport à la taille de la liste, soit O(n). 


*)