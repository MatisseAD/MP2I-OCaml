let n () = 4

let obstacles_lignes =
  [|
    [| 0; 4; 10; 16 |];
    [| 0; 14; 16 |];
    [| 0; 6; 16 |];
    [| 0; 9; 16 |];
    [| 0; 3; 15; 16 |];
    [| 0; 7; 16 |];
    [| 0; 1; 12; 16 |];
    [| 0; 7; 9; 16 |];
    [| 0; 7; 9; 16 |];
    [| 0; 4; 13; 16 |];
    [| 0; 6; 16 |];
    [| 0; 10; 16 |];
    [| 0; 8; 16 |];
    [| 0; 2; 15; 16 |];
    [| 0; 4; 10; 16 |];
    [| 0; 5; 12; 16 |];
  |]

let obstacles_colonnes =
  [|
    [| 0; 5; 11; 16 |];
    [| 0; 6; 13; 16 |];
    [| 0; 4; 16 |];
    [| 0; 15; 16 |];
    [| 0; 10; 16 |];
    [| 0; 3; 16 |];
    [| 0; 10; 16 |];
    [| 0; 6; 7; 9; 12; 16 |];
    [| 0; 7; 9; 16 |];
    [| 0; 3; 12; 16 |];
    [| 0; 14; 16 |];
    [| 0; 16 |];
    [| 0; 7; 16 |];
    [| 0; 2; 10; 16 |];
    [| 0; 4; 13; 16 |];
    [| 0; 2; 12; 16 |];
  |]

let dichotomie (p : int) (t : int array) : int =
  (** On cherche le plus petit indice i tel que t.(i) <= p < t.(i+1)*)
  let n = Array.length t in
  let a = ref 0 in
  let b = ref n in
  let ans = ref (-1) in
  while !b - !a > 1 do
    let m = (!b + !a) / 2 in
    if p < t.(m) then 
      b := m
    else if p > t.(m) then
      begin
      a := m;
      ans := m
      end
    else begin
      ans := m
    end
  done;
  if t.(!a) <= p && t.(!b) > p then
    !ans
  else
    !b

let deplacements_grille (a, b) =
  let ans = Array.make 4 (0,0) in
  let checkObstacles (newa, newb) = 
    let x = a in
    let y = b in
    let estPossible = ref true in
    for i = 0 to (Array.length (obstacles_colonnes.(x)) - 1) do
      begin
        if obstacles_colonnes.(x).(i) = newa then
          estPossible := false
        end
      done;
    for i = 0 to (Array.length (obstacles_lignes.(y))- 1) do
      if obstacles_lignes.(y).(i) = newb then
        estPossible := false
      done;
      in
      let _ = checkObstacles (0, 0) in
      ans

let rec insertion x q =
  match q with
  | [] -> [x]
  | a :: abs ->
    if x > a then
      a :: insertion x abs
    else
      x :: a :: abs

let rec tri_insertion q =
  match q with
  | [] -> []
  | x :: xs -> insertion x (tri_insertion xs)

let rec assoc (x : 'a) (q : ('a*'b) list) =
  match q with
  | [] -> failwith "Aucun couple existe"
  | (a,b) :: t ->
    if x = a then
      b
    else
      assoc x t