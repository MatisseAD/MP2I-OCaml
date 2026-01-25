let rec ajoute (x : int) (u: int list) : int list =
  match u with
  | [] -> []
  | h :: t -> h + x :: ajoute x t

  let rec sommes_cumulees (u : int list) : int list =
    match u with
    | [] -> []
    | h :: t -> h :: ajoute h (sommes_cumulees t) 

let rec sans_doublons u =
  match u with
  | [] -> true
  | x :: t -> if List.mem x t then false else sans_doublons t

let rec elimine_doublons u =
  match u with
  | [] -> []
  | x :: t -> 
    if List.mem x t then elimine_doublons t
    else x :: elimine_doublons t