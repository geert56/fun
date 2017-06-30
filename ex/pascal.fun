/* Pascal's triangle */

let pascal = iterate (L row . zipwith + ([0] `++` row) (row `++` [0])) [1];

/*
letrec showl l = if (null l) []
                 [ show (car l) : showl (cdr l) ];

let showPascal = (layn . map showl . take 14) pascal;
*/
