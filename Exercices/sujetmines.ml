let mat_bool a b =
  let d = Array.length a in
  let ans = Array.make_matrix d d false in
  for i = 0 to d - 1 do
    for j = 0 to d - 1 do
      let cpt = ref 0 in
      for k = 0 to d - 1 do
        match (a.(i).(k), b.(k).(j)) with
        | false, false -> ()
        | false, true -> ()
        | true, false -> ()
        | true, true -> cpt := !cpt + 1
      done;
      if !cpt <> 0 then ans.(i).(j) <- true else ans.(i).(j) <- false
    done
  done;
  ans
