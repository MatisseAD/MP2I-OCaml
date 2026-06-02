let miroir (g : int list array) : int list array =
  let n = Array.length g in
  let m = Array.make n [] in
  let rec aux (l : int list) (t : int) : unit =
    match l with
    | [] -> ()
    | x :: xs ->
        m.(x) <- t :: m.(x);
        aux xs t
  in
  for i = 0 to n - 1 do
    aux g.(i) i
  done;
  m

let g =
  [|
    [ 1; 2 ];
    [ 2; 3; 4 ];
    [];
    [ 0; 5 ];
    [ 1; 2 ];
    [ 10 ];
    [ 1; 9 ];
    [ 8 ];
    [ 6 ];
    [ 7; 10 ];
    [ 11 ];
    [ 5 ];
  |]
