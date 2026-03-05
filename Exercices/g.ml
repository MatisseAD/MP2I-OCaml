let russian_roulette (n : int) (player1_choose : int) (player2_choose : int) =
  let rec aux n player1_choose player2_choose =
    if n = 0 then "Player 1 wins!"
    else if n = 1 then "Player 2 wins!"
    else
      let bullet_position = Random.int n in
      if bullet_position = player1_choose then "Player 1 loses!"
      else if bullet_position = player2_choose then "Player 2 loses!"
      else aux (n - 1) player1_choose player2_choose
  in
  aux n player1_choose player2_choose
