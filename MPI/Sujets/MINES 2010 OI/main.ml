(** let F3 : int array array =
    [|[|3;4|];[|4;1;-2;3;4|];[|2;-1;3|];[|3;1;-4;2|]|];; **)

let valeur_clause (c : int array) (value : int array) =
  let ansb = ref true in
  let i = c.(0) in
  for j = 1 to i do
    if
      (not (value.(abs c.(j)) = 0 && c.(j) < 0))
      or (value.(abs c.(j)) = 1 && c.(j) > 0)
    then ansb := false
  done;
  !ansb

let satisfait_formule f value =
  let i = f.(0).(0) in
  let ans = ref true in
  for j = 1 to i do
    if not (valeur_clause f.(j) value) then ans := false
  done;
  !ans

let transfo_f nbval_arr n =
  let power2 = ref 1 in
  let cpt = ref 0 in
  let i = ref n in
  let f = Array.make (n + 1) 0 in
  f.(0) <- nbval_arr.(0);
  for i = n downto 1 do
    cpt := !cpt + (!power2 * nbval_arr.(i));
    power2 := !power2 * 2
  done;
  if !cpt = !power2 - 1 then nbval_arr
  else (
    cpt := !cpt + 1;
    while !cpt <> 0 && !i >= 1 do
      let reste = !cpt mod 2 in
      cpt := !cpt / 2;
      f.(!i) <- reste;
      decr i
    done;
    f)

let rec resoudre_rec f value : int array =
  if value.(0) = 0 then value.(0) <- 1;
  let n = f.(0).(1) in
  if satisfait_formule f value then value
  else
    let r = transfo_f value n in
    if r = value then begin
      value.(0) <- 0;
      value
    end
    else resoudre_rec f r

let resoudre f : int array =
  let nb_val = f.(0).(1) in
  let valeur = Array.make (nb_val+1) 0 in
  resoudre_rec f valeur

(** Q12) 

La complexité est en O(m2^n)

*)

(** Q13)

    cf. pdf *)

(** Q14 *)

let place (c : int array) (litt : int) =
  let ans = ref (false, 0) in
  let n = c.(0) in
  for i = 1 to n do
    if c.(i) = litt || c.(i) = -litt then ans := (true, i)
  done;
  snd !ans

let supprimer_variable (c : int array) (i : int) =
  let n = c.(0) in
  c.(0) <- c.(0) - 1;
  c.(i) <- c.(n)

let supprimer_clause (f : int array array) (i : int) =
  let nb_clause = f.(0).(0) in
  f.(0).(0) <- nb_clause - 1;
  f.(i) <- f.(nb_clause)

let calculer_diff (f : int array array) : int array =
  let n = f.(0).(1) in
  let ans = Array.make (n + 1) 0 in
  let j = f.(0).(0) in
  for litt = 1 to n do
    for a = 1 to j do
      let l = place f.(a) litt in
      if l <> 0 then
        match f.(a).(l) with
        | x when x < 0 -> ans.(litt) <- ans.(litt) - 1
        | _ -> ans.(litt) <- ans.(litt) + 1
    done
  done;
  ans

let simplifier f alpha v =
  let ans = ref 0 in
  let litt = ref 0 in
  if v = 1 then litt := alpha else litt := -alpha;
  let nb_clause = f.(0).(0) in
  let i = ref 1 in
  while !i <> nb_clause do
    let pi = place f.(!i) !litt in
    if pi <> 0 then
      if f.(!i).(pi) = !litt then (
        incr ans;
        supprimer_clause f !i
        (** On n'incrémente pas i pour vérifier la nouvelle clause*)
        )
      else 
        supprimer_variable f.(!i) pi;
        incr i
  done;
  !ans

let max_i tab n =
  let i_max = ref 1 in
  for i = 2 to n do
    if abs tab.(!i_max) < abs tab.(i) then i_max := i
  done;
  !i_max

(** Si :

    - La clause est supprimé puisque la variable est égal à 1 => + 1 au cpt car
      la clause est satisfaite
    - Sinon ne rien faire *)

let heuristique f =
  let nb_var = f.(0).(1) in
  let n = nb_var + 1 in
  let ans = Array.make (nb_var + 1) 0 in
  let diff = calculer_diff f in
  let i = ref 1 in
  while !i <> nb_var do
    let j = ref (max_i diff n) in
    let v = if diff.(!j) < 0 then -1 else 1 in
    let c = simplifier f !j v in
    ans.(!j) <- c;
    ans.(0) <- ans.(0) + c;
    diff.(!j) <- min_int;
    incr i
  done;
  ans
