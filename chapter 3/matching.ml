let x =
  match not true with 
  | true -> "nope"
  | false -> "yep"

let y = 
  match 42 with
  | fooo -> fooo

let z = 
  match "foo" with
  | "bar" -> 0
  | _ -> 1

let a =
  match [] with
  | [] -> "empty"
  | _ -> "not empty"

let b = 
  match ["taylor"; "swift"] with
  | [] -> "folklore"
  | h :: t -> h

let fst3 t =
  match t with 
  | (a, b, c) -> a

let empty = function
  | [] -> true
  | _ -> false

let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t


let rec length = function
  | [] -> 0
  | h :: t -> length t + 1

(** 
  Fonction append : Concatène deux listes en une seule liste
  
  Principe : On reconstruit la première liste élément par élément,
  en ajoutant la deuxième liste à la fin.
  
  @param lst1 La première liste
  @param lst2 La deuxième liste (qui sera ajoutée à la fin de lst1)
  @return Une nouvelle liste contenant tous les éléments de lst1 suivis de tous les éléments de lst2
  
  Exemple : append [1;2] [3;4] -> [1;2;3;4]
  
  Trace d'exécution pour append [1;2] [3;4] :
  - append [1;2] [3;4]
  - 1 :: (append [2] [3;4])
  - 1 :: (2 :: (append [] [3;4]))
  - 1 :: (2 :: [3;4])
  - 1 :: [2;3;4]
  - [1;2;3;4]
*)
let rec append lst1 lst2 = 
  match lst1 with
  | [] -> lst2                        (* Cas de base : si lst1 est vide, retourner lst2 *)
  | h :: t -> h :: (append t lst2)   (* Cas récursif : prendre le premier élément h de lst1,
                                         et le mettre devant le résultat de append t lst2 *)

