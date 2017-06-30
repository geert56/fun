/* Hamming problem see examples.gs in Gofer. */

letrec hamming = [ 1 :      map (2 `*`) hamming
                       `||` map (3 `*`) hamming
                       `||` map (5 `*`) hamming ]
                 where || xs ys =
                   if (== (car xs) (car ys)) [ car xs : || (cdr xs) (cdr ys) ]
                  (if (<  (car xs) (car ys)) [ car xs : || (cdr xs) ys ]
                                             [ car ys : || (cdr ys) xs ])
                 end;


