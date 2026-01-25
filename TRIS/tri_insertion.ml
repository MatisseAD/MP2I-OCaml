let tri_insertion (t : int array) : int array = 
  let j = ref 0 in
  for i=1 to Array.length t - 1 do
    j := i;
    if t.(i) < t.(i-1) then
      while !j >= 0 && t.(!j) < t.(!j-1) do
        let tampon = t.(!j-1) in
        t.(!j-1) <- t.(!j);
        t.(!j) <- tampon;
        j := !j -1;
      done;
  done;
  t;;
