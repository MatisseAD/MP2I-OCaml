let rec prepend (x : 'a) (u : 'a list list) =
  match u with
  | [] -> []
  | h :: t -> (x :: h) :: prepend x t

  (** Exercice A5 *)

let rec renverse (u : 'a list) : 'a list =
  match u with
  | [] -> []
  | x :: xs -> (renverse xs) @ [x]

exception ERRRRRRROORRR

let rec renverse_dans u v =
  match u with
  | [] -> v
  | x :: xs -> renverse_dans xs (x :: v)


let rec renverse2 u = 
  renverse_dans u []

