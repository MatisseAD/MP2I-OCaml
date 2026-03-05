type 'a arbre = F of 'a | N of 'a * 'a arbre * 'a arbre

let rec hauteur (a : 'a arbre) : int =
  match a with F _ -> 0 | N (_, g, d) -> 1 + max (hauteur g) (hauteur d)

let rec profondeur_min (a : 'a arbre) : int =
  match a with
  | F _ -> 1
  | N (_, g, d) -> 1 + min (profondeur_min g) (profondeur_min d)

let rec profondeur_max (a : 'a arbre) : int =
  match a with
  | F _ -> 1
  | N (_, g, d) -> 1 + max (profondeur_max g) (profondeur_max d)

let rec diff_max (a : 'a arbre) : int = profondeur_max a - profondeur_min a
let m : 'a arbre = N (1, N (2, N (3, F 4, F 5), N (5, F 6, F 7)), F 1)

let rec feuille_basse (a : 'a arbre) : 'a =
  match a with
  | F x -> x
  | N (_, g, d) -> (
      match (g, d) with
      | F x, F _ -> x
      | F _, N (_, g_prime, d_prime) ->
          let diff_g_prime = diff_max g_prime in
          let diff_d_prime = diff_max d_prime in
          if diff_g_prime > diff_d_prime then feuille_basse g_prime
          else feuille_basse d_prime
      | N (_, g_prime, d_prime), F _ ->
          let diff_g_prime = diff_max g_prime in
          let diff_d_prime = diff_max d_prime in
          if diff_g_prime > diff_d_prime then feuille_basse g_prime
          else feuille_basse d_prime
      | N (_, dd, gg), N (_, d_prime, g_prime) ->
          let diff_dd = diff_max dd in
          let diff_gg = diff_max gg in
          let diff_d_prime = diff_max d_prime in
          let diff_g_prime = diff_max g_prime in
          if diff_dd > diff_gg then
            if diff_dd > diff_d_prime then
              if diff_dd > diff_g_prime then feuille_basse dd
              else feuille_basse g_prime
            else feuille_basse d_prime
          else feuille_basse d_prime)
