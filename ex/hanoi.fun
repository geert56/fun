/*
let hanoi n = hanoi1 n 1 2 3
  where hanoi1 n s d u =
    if (> n 0)
      (seq3 (hanoi1 (- n 1) s u d)
	    (move n s d)
	    (hanoi1 (- n 1) u d s))
      Ready
    where move n s d =
      stderr ("Move disk " $++ (show n) $++ " from pole " $++ (show s)
             $++ " to pole " $++ (show d) $++ "\n"),
          seq3 a b c = seq a (seq b (seq c Ready))
    end
  end;
*/

let _hanoi n = hanoi1 n 1 2 3
  where hanoi1 n s d u =
    if (> n 0)
      (    (hanoi1 (- n 1) s u d)
       $++ [[ n, s, d ]]
       $++ (hanoi1 (- n 1) u d s))
      []
  end;

let hanoi n = seq (stderr (showhanoi (_hanoi n))) (len (_hanoi n))
  where showhanoi l =
    if (null l) ""
      (++ (showmove (car l)) (showhanoi (cdr l)))
    where showmove l =
          "Move disk "
      $++ (show (ith 0 l))
      $++ " from pole "
      $++ (show (ith 1 l))
      $++ " to pole "
      $++ (show (ith 2 l))
      $++ "\n"
    end
  end;
