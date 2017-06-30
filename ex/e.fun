/* Digits of e = 2.718281828...; see examples.gs in Gofer. */

type      e :: [Int];
type  scale :: [Int] -> [Int];
type renorm :: [Int] -> [Int];
type   step :: (Int,Int) -> [Int] -> [Int];

let e = map head (iterate scale [2:repeat 1])
  where scale = renorm . map (10 `*`) . tail
    where renorm ds = foldr step [0] (zip ds [2..])
      where step dn bs =
        (if ((d `mod` n `+` 9) `<` n) [(d `/` n), b : tail bs]
         [c, b : tail bs])
  	where d  = fst dn,
              n  = snd dn,
              b' = head bs,
              b  = (d `+` b') `mod` n,
              c  = (d `+` b') `div` n
        end
      end
    end
  end;


/* Explanation:

   e = SUM 1/n! = 1/1 + 1/1 + 1/2 + 1/6 + 1/24 + ...
       n=0

   [2:repeat 1] = [2,1,1,1,...]
   iterate scale [2:repeat 1] =
     [ [2:repeat 1], scale [2:repeat 1], scale (scale [2:repeat 1]), ...]
   map head (iterate scale [2:repeat 1]) =
     [ 2, head (scale [2:repeat 1]), head (scale (scale [2:repeat 1])), ...]
   scale [2:repeat 1] =
     renorm [10,10,10,...] = foldr step [0] [ (10,2), (10,3), (10,4), ...] =
       step (10,2) (step (10,3) (step (10,4) (step ...))) =
         and then what ???
*/
