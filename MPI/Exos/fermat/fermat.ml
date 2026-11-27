let rec pow_mod_p (a : int) (p : int) (mod_p : int) : int =
  let ans = ref 1 in
  for i = 1 to p do
    ans := !ans mod mod_p * (a mod mod_p) mod mod_p
  done;
  !ans mod mod_p

let fermat (a : int) (p : int) : bool =
  let a' = pow_mod_p a (p - 1) p in
  let r = a' mod p in
  r = 1

let main (p : int) : bool =
  let a = 2 + Random.int (p - 2) in
  fermat a p

let test : float =
  let p = 1729 in
  let nb_tests = 100000 in
  let proba = ref 0 in
  for i = 1 to nb_tests do
    let ans = main p in
    if ans then (
      let msg =
        Printf.sprintf "Le test numéro %d passe, p est probablement premier" i
      in
      incr proba;
      print_string msg;
      print_newline ())
    else
      let msg =
        Printf.sprintf "Le test numéro %d ne passe pas, p n'est pas premier" i
      in
      print_string msg;
      print_newline ()
  done;
  float_of_int !proba /. float_of_int nb_tests
