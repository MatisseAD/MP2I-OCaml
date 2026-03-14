(** Exercice 4.1 **)

let affiche_pile (pile : int Stack.t) : unit =
  while not (Stack.is_empty pile) do
    let a = Stack.pop pile in
    print_int a
  done

(** Après éxécution de cette fonction, la pile sera malehuresement vide ... **)

let rec liste_to_pile (liste : 'a list) (pile : 'a Stack.t) : unit =
  match liste with
  | x :: xs ->
      Stack.push x pile;
      liste_to_pile xs pile
  | [] -> ()

let rec pile_to_liste (pile : 'a Stack.t) : 'b list =
  if not (Stack.is_empty pile) then
    let x = Stack.pop pile in
    x :: pile_to_liste pile
  else []

let iter_pile f p =
  let p_list = pile_to_liste p in
  List.iter f p_list

let show_elements p =
  let f x = print_int x in
  iter_pile f p

let sum_pile p =
  let cpt = ref 0 in
  let f x = cpt := x + !cpt in
  iter_pile f p

(** Exercice 4.2 **)

(** Q1

    CF chapitre4.md *)

let engendrable (t : int list) : bool =
  let stack = Stack.create () in
  Stack.push 1 stack;
  let rec aux (t : int list) (value_str : string) (cpt : int) =
    match t with
    | [] -> value_str
    | x :: xs ->
        let top = Stack.top stack in
        if top < x then begin
          Stack.push (cpt + 1) stack;
          aux (x :: xs) (value_str ^ "E") (cpt + 1)
        end
        else if top > x then begin
          aux xs (value_str ^ "F") cpt
        end
        else begin
          let _ = Stack.pop stack in
          aux xs (value_str ^ "D") cpt
        end
  in
  let ans = aux t "E" 1 in
  if String.contains ans 'F' then false
  else (
    print_string ans;
    true)
