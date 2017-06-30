/* `Pure' lambda calculus.

   "Programming Language Theory and its Implementation,"
   Michael J.C. Gordon,
   Prentice-Hall, 1988
*/

let IF    = L c t e . c t e;

let FALSE = L x y . y;			/* = K I */
let TRUE  = L x y . x;			/* = K */
let NOT   = L t . t FALSE TRUE;
let AND   = L x y . IF x y FALSE;
let OR    = L x y . IF x TRUE y;

let FST = L p . p TRUE;
let SND = L p . p FALSE;
let PAIR E1 E2 = L f . f E1 E2;

/* Number representation according to Church: */
let ZERO  = L f x . x;
let SUC   = L n f x . n f (f x);
let ADD   = L m n f x . m f (n f x);
let ZEROP = L n . n (L x . FALSE) TRUE;
