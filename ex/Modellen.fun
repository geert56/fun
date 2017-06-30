/* Course: "Modellen voor Digitale Systemen" */

/* Original definitions:

   let F X   = if (null X) []
               (otherwise   [ car X ]);
   let R X   = cdr X;
   let A X Y = if (null X) []
               (if (null Y) [ car X ]
               (otherwise   [ car X : Y ]));

   When resorting to infinite streams must not use any conditional
   functions like the ones above but must use these:
*/
let F X   = [ car X ];	 	/* A singleton list of X's first element. */
let R X   = cdr X;		/* X but its first element. */
let A X Y = [ car X : Y ];	/* First of X concatenated with Y. */

/* The `function' nodes: */
letrec f U V = A U (A V (f (R U) (R V)));
letrec g1 U  = A U (g1 (R (R U)));
letrec g2 U  = A (R U) (g2 (R (R U)));
let h U X    = A [ X ] U;
/*let h U X    = U;*/

/* A simple Kahn network (connections of function nodes): */
letrec X = f Y Z,
       Y = h T1 0,
       Z = h T2 1,
       T1 = g1 X,
       T2 = g2 X;
