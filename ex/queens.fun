/* n queens problem solved in Miranda:
   (Attributed to Turner)

queens 0 = [ [0] ]
queens n = [ q:b | q <- [0..7]; b <- queens(n-1); safe q b ], n > 0
safe q b = and [ ~checks q b i | i <- [0..#b-1] ]
checks q b i = q=b!i \/ abs(q - b!i) =i+1

There are errors in the above definition!

	N:	#Solutions:
	1	1
	2	0
	3	0
	4	2
	5	10
	6	4
	7	40
	8	92
	9	352
	10	724
	11	2680
	12	14200
*/

/* Returns list of all solutions of n-queens problem, i.e., the board
   is n by n squares. (n >= 1).
   Each solution is itself a list of length n with numbers indicating
   the column (or row, if you like) positions (counted from 1) of the queens.
*/
let queens n = queens1 n n
  where queens1 M n =
    if (n `==` 1)
      [ [x] | x <- [1..M] ]
     [ [q:b] | q <- [1..M]; b <- queens1 M (n `-` 1); safe q b ]
    where safe q b = and [ not (checks q b i) | i <- [1..len b] ]
      where checks q b i = let r = ith (- i 1) b in
	(q `==` r) `||` ((abs (- q r)) `==` i)
      end
    end
  end;

let showboard s = showrows s (len s)
  where showrows s n = if (null s) ""
          (++ (showrow (car s) n) (showrows (cdr s) n))
    where showrow r n =
      ++ (++ (++ (spaces (- r 1)) "q") (spaces (- n r))) "\n"
      where spaces n = if (== n 0) "" (++ " " (spaces (- n 1)))
      end
    end
  end;
