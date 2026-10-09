type file_prior = { mutable taille : int; contenu : (int * int) array }
type 'a file = { mutable g : 'a list; mutable d : 'a list }

let echange t i j =
  let s = t.(i) in
  begin
    t.(i) <- t.(j);
    t.(j) <- s
  end

let push_file x f = f.d <- x :: f.d

let pop_file f =
  if f.g <> [] then (
    let t = List.hd f.g in
    f.g <- List.tl f.g;
    t)
  else begin
    while f.d <> [] do
      let t = List.hd f.d in
      f.g <- t :: f.g;
      f.d <- List.tl f.d
    done;
    let t = List.hd f.g in
    f.g <- List.tl f.g;
    t
  end

let init_file () = { g = []; d = [] }
let est_vide_file f = f.g = [] && f.d = []

let push_prior file e =
  let n = file.taille in
  let contenu = file.contenu in
  contenu.(n) <- e;
  let target = ref n in
  while !target > 0 do
    let pere = (!target - 1) / 2 in
    let _, prio = contenu.(!target) in
    let _, prio_pere = contenu.(pere) in
    if prio < prio_pere then begin
      echange contenu !target pere;
      target := pere
    end
    else target := 0
  done;
  file.taille <- n + 1

let pop_prior file =
  let nprev = file.taille in
  if nprev = 0 then invalid_arg "pop_prior"
  else
    let contenu = file.contenu in
    let ans = contenu.(0) in
    echange contenu (nprev - 1) 0;
    file.taille <- file.taille - 1;
    let n = file.taille in
    let i = ref 0 in
    while (2 * !i) + 1 < n do
      let gauche = (2 * !i) + 1 in
      let droite = gauche + 1 in
      let meilleur =
        if droite < n && snd contenu.(droite) < snd contenu.(gauche) then droite
        else gauche
      in
      if snd contenu.(meilleur) < snd contenu.(!i) then begin
        echange contenu !i meilleur;
        i := meilleur
      end
      else i := n
    done;
    ans

(**let tri t =
  let n = Array.length t in
  let contenu = Array.make n (0, 0) in
  let file_prio = { taille = 0; contenu } in
  for i = 0 to n - 1 do
    push_prior file_prio (t.(i), t.(i))
  done;
  for i = 0 to n - 1 do
    let e = pop_prior file_prio in
    t.(i) <- e
  done**)

let est_vide_prior file = if file.taille = 0 then true else false

(******************* Et pour finir : Dijkstra ! ***********************)

let djikstra (g : (int * int) list array) (s : int) : int array =
  let n = Array.length g in
  let deja_vu = Array.make n false in
  let distance = Array.make n max_int in
  distance.(s) <- 0;
  let file =
    { taille = 0; contenu = Array.make (max 1 ((n * n) + 1)) (0, 0) }
  in
  push_prior file (0, 0);
  while not (est_vide_prior file) do
    let e = pop_prior file in
    let indice, prio = e in
    if not deja_vu.(indice) then begin
      deja_vu.(indice) <- true;
      distance.(indice) <- prio;
      List.iter
        (fun x ->
          let a, b = x in
          if not deja_vu.(a) then push_prior file (a, b + prio))
        g.(indice)
    end
  done;
  distance

let graphe_exp =
  [|
    [ (1, 2); (4, 5); (2, 1) ];
    [ (0, 2); (4, 6); (1, 3) ];
    [ (0, 3); (4, 3); (1, 2) ];
    [];
    [ (0, 5); (2, 5); (1, 6) ];
  |]
