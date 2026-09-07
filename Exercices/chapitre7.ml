(** Exercice 7.2 Cours*)

type form =
  | V
  | F
  | Var of int
  | Neg of form
  | Imp of form * form
  | Ou of form * form
  | Ssi of form * form
  | Et of form * form

type valuation = int array

let evaluation (v : valuation) (f : form) : int =
  let rec aux (f : form) : bool =
    match f with
    | V -> true
    | F -> false
    | Var x -> if v.(x) = 0 then false else true
    | Neg x -> not (aux x)
    | Imp (x, y) -> aux y || not (aux x)
    | Ou (x, y) -> aux x || aux y
    | Et (x, y) -> aux x && aux y
    | Ssi (x, y) -> aux x = aux y
  in
  if aux f then 1 else 0

let est_model (v : valuation) (f : form) : bool =
  if evaluation v f = 1 then true else false

let est_equivalente (f1 : form) (f2 : form) (n : int) : bool =
  let v = Array.make n 0 in
  let rec aux i : bool =
    if i = n then evaluation v f1 = evaluation v f2
    else begin
      v.(i) <- 0;
      let r1 = aux (i + 1) in
      v.(i) <- 1;
      let r2 = aux (i + 1) in
      r1 = r2
    end
  in
  aux 0

let est_tautologie (f : form) (n : int) : bool =
  let v = Array.make n 0 in
  let rec aux i : bool =
    if i = n then if evaluation v f = 1 then true else false
    else begin
      v.(i) <- 0;
      let r1 = aux (i + 1) in
      v.(i) <- 1;
      let r2 = aux (i + 1) in
      r1 && r2
    end
  in
  aux 0

(** Exercice 7.1 *)

(** On additionne 'ab' et 'cd', deux nombres de deux chiffres en binaires. Ainsi
    'ab' + 'cd' = 'pqr' où 'pqr' est un nombre en binaire

    r = b xor d *)

(** Exercice 7.3 *)

type proposition =
  | Var of string
  | Vrai
  | Faux
  | Neg of proposition
  | Donc of proposition * proposition
  | Et of proposition * proposition
  | Ou of proposition * proposition

type ifExpr =
  | Var_ite of string
  | Vrai_ite
  | Faux_ite
  | Ite of ifExpr * ifExpr * ifExpr
