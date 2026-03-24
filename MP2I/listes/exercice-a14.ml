let rec range (a :int) (b : int) : int list =
  if a - b = 0 then [] else a :: range (a+1) b

let rec concat l1 l2 =
  match l1 with
  | [] -> l2
  | [w] -> w :: l2
  | h :: t -> h :: concat t l2
  
let rec flatten (u : 'a list list) : 'a list =
  match u with
  | [] -> []
  | h :: t -> concat h (flatten t)

let rec enumere a b = 
  