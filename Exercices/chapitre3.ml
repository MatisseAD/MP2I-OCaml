(**Exercice 3.1 **)

let rec suite (n : int) =
  match n with
  | 0 -> [0]
  | p -> 
    let rec filtre (l : int list) =
      match l with
      | [] -> []
      | x :: xs -> if x = 0 then 1 :: 0 :: filtre xs else 0 :: 1 :: filtre xs
    in filtre (suite (p-1))

(** Exercice 3.2*)

let rec coupure liste n =
  match n, liste with
  | _,[] -> (liste,[])
  | 1,x::xs -> ([x], xs)
  | p,x::xs -> let a,b = coupure xs (p-1) in
  (x :: a, b)

let shuffle l1 l2 =
  let rec aux l1 l2 turn =
    match l1,l2 with
    | _,[] -> l1
    | [],_ -> l2
    | x::xs,y::ys ->  if turn = 0 then x :: aux xs (y::ys) 1 else y :: aux (x::xs) ys 0
  in 
  if List.length l1 > List.length l2 then aux l1 l2 0 else aux l1 l2 1

(** Exercice 3.3 *)

let estsousliste l1 l2 =
  if List.length l1 > List.length l2 then
    false
  else
    let rec aux l1 l2 =
      match l1,l2 with
      | _,[] -> false
      | [],_ -> true
      | x::xs,y::ys -> if x = y then aux xs ys else aux (x::xs) ys
    in aux l1 l2

let listedessouslistes l =
  let rec aux2 l x =
    match l with
    | [] -> true
    | y::ys -> if [y] <> x && ys <> x then aux2 ys x else false
  in
  let rec aux l =
    match l with
    | [] -> []
    | [x] -> []
    | x::xs -> 
      let rest = aux xs in
      if aux2 rest [x] then
        if List.mem xs rest then
          [x] :: xs :: rest
        else
          [x] :: rest
      else
        rest
  in aux l


(** Exercice 1;2;3;4;8;9;12;14*)