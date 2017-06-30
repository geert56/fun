/* Power set. */

/*
letrec powset A = if (null A) [ [] ]
                  let P = powset (cdr A) in
                    ++ P [ [ car A : p ] | p <- P ];
*/

letrec powset A = if (null A) [ [] ]
		  (powset (cdr A) $++ map [car A:] (powset (cdr A)));

