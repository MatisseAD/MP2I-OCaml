let expo_rapide x n =
  let nn = ref n in
  let xx = ref x in
  let res = ref 1 in
  while !nn > 0 do
    if !nn mod 2 = 1 then
      res := !res * !xx;
    nn := !nn / 2;
    xx := !xx * !xx;
    done;
  !res;;