type 'a bintree = E | N of 'a bintree * 'a * 'a bintree

let rec cardinaux b =
  match b with
  | E -> (E, 0)
  | N (g, x, d) ->
      let a1, c1 = cardinaux g in
      let a2, c2 = cardinaux d in
      (N (a1, x, a2), c1 + c2 + 1)
