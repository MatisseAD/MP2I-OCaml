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

(** Principe de backtracking *)

(** Problème à résoudre :

    Si on a un cas [|1;1;1;1;1;1|] Et que cela ne marche pas, on ne revient pas
    au début pour tester un truc du style :

    [|1;1;0;1;1|] *)

(** let rec resoudre_rec f value (k : int) : int array = if k = 0 then begin if
    value.(0) <> 1 then begin value.(0) <- 1; resoudre_rec f value (f.(0).(1))
    end else begin value.(0) <- 0; value end end else begin let nb_var =
    f.(0).(1) in if satisfait_formule f value then value else if value.(k) = 1
    && k <> nb_var then begin if value.( end **)

let resoudre f : int array =
  let nb_val = f.(0).(1) in
  let valeur = Array.make nb_val 0 in
  resoudre_rec f valeur 0

(** Q12) *)

(** Q13) *)

(** Q14 *)

let place (c : int array) (litt : int) =
  let ans = ref (false, 0) in
  let n = c.(0) in
  for i = 1 to n do
    if c.(i) = litt then ans := (true, i)
  done;
  snd !ans

let supprimer_variable (c : int array) (i : int) =
  let n = c.(0) in
  c.(0) <- c.(0) - 1;
  c.(i) <- c.(n)

let supprimer_clause (f : int array array) (i : int) =
  let nb_clause = f.(0).(0) in
  f.(0).(0) <- nb_clause - 1;
  f.(i).(0) <- f.(nb_clause).(0)

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
