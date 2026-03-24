type 'a trie = {
  mutable value : 'a option;
  branches : (char, 'a trie) Hashtbl.t;
}

let create () = { value = None; branches = Hashtbl.create 0 }

let put (t : 'a trie) (s : string) (v : 'a) : unit =
  let rec add (t : 'a trie) (i : int) : unit =
    let n = String.length s in
    if i = n then t.value <- Some v
    else
      let b =
        if not (Hashtbl.mem t.branches s.[i]) then
          Hashtbl.add t.branches s.[i] (create ());
        Hashtbl.find t.branches s.[i]
      in
      add b (i + 1)
  in
  add t 0

let get (t : 'a trie) (s : string) : 'a =
  let n = String.length s in
  let rec find t i =
    if i = n then
      match t.value with None -> failwith "Not Found" | Some v -> v
    else find (Hashtbl.find t.branches s.[i]) (i + 1)
  in
  find t 0

let remove t s =
  let n = String.length s in
  let rec find t i =
    if i = n then match t.value with Some v -> t.value <- None | _ -> ()
    else find (Hashtbl.find t.branches s.[i]) (i + 1)
  in
  find t 0
