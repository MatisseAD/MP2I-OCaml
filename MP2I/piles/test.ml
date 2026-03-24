type 'a stack = 'a list ref

let empty_stack () : 'a stack = ref []
let push (x : 'a) (s : 'a stack) : unit = s := x :: !s

let pop (s : 'a stack) : 'a =
  match !s with
  | [] -> failwith "Impossible"
  | x :: xs ->
      s := xs;
      x

let is_empty (s : 'a stack) : bool = match !s with [] -> true | _ -> false

let rotation (s : 'a stack) =
  let rec aux2 (p : 'a stack) (f : 'a stack) : 'a stack =
    if not (is_empty p) then (
      let a = pop p in
      let result = aux2 p f in
      push a result;
      result)
    else f
  in
  let aux = aux2 s (empty_stack ()) in
  while not (is_empty aux) do
    let a = pop aux in
    push a s
  done
