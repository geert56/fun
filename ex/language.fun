/* Copyright (c) 1995 G. Janssen */

/* An alphabet Sigma is a fun list (e.g. of characters).
   A `string' (= sequence of symbols) is a fun list of alphabet symbols.
   The empty `string' is denoted by [];
   A language is a fun list of `strings'.
   The empty language is also [].
*/

/* Raising an alphabet Sigma to the power i >= 0.
   (Powers of a set; repeated carthesian product; size of result is |Sigma|^i).
   Order of strings in the result is such that first all strings with
   (ith 0 Sigma) appear, then the strings that start with (ith 1 Sigma),
   et cetera.
   (Note:    [] $\^ 0 = [ [] ],
             [] $\^ i = [], i > 0
          Sigma $\^ 0 = [ [] ],
          Sigma $\^ 1 = map enlist Sigma.)
*/
letrec \^ Sigma i = if (== i 0) [ [] ]
                    [ [x:y] | x <- Sigma; y <- \^ Sigma (- i 1) ];

/* Kleene closure of alphabet Sigma.
   (Union of all powers:  U   Sigma^i. Note: \* [] = [ [] ])
                         i>=0
   Order of strings in the result is such that first is the empty string,
   then all strings of length 1, followed by the strings of length 2,
   et cetera. Order of strings of equal length is as described above.
*/
let \* Sigma = if (null Sigma) [ [] ] (gen Sigma 0)
  where gen Sigma i = ++ (\^ Sigma i) (gen Sigma (+ i 1)) end;
