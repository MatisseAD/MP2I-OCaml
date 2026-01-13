(** https://cs3110.github.io/textbook/chapters/data/exercises.html *)

(** Exercice 2*)

let rec product = function
  | [] -> 1
  | h :: t -> h * product t


(** Exercice 1*)

let one_to_five = [1;2;3;4;5]

let one_to_five_1 = 1 :: 2 :: 3 :: 4 :: 5 :: []

let two_to_four = [2] @ [3] @ [4]

let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t