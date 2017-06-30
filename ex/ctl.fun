/* Interpreting CTL operators over sets of states. */

/* The set of states (must be numbers, chars, or names): */
let Q = [0..5];

/* Used by greatest-fixpoint function: */
let Universe = Q;

/* A sample next-state function R' : Q -> 2^Q */
let R' s = ith s [ [1,3], [1,3], [1,4], [2,4], [5], [5] ];

/* Extended next-state function R : 2^Q -> 2^Q */
let R V = nodups = (sort <= (foldl ++ [] [ R' s | s <- V ]));

/* Returns True if sets A and B have at least 1 element in common: */
letrec intersects A B = if (null A)           False
                       (if (null B)           False
                       (if (member (car A) B) True
                       (otherwise             (intersects (cdr A) B))));

/* Assumes elements of A contain no duplicates. */
let intersect A B = [ e | e <- A; e $member B ];
let union A B = nodups = (sort <= (++ A B));

/* The converse of a function 2^Q -> 2^Q: */
/* Note: converse already defined in stddefs.fun. */
undef converse;
let converse R V = [ t | t <- Q; intersects (R [t]) V ];

/* The Image of R: */
let Image = converse R;

load "Fix.fun";

let EX V   = Image V;
let EG V   = GFP (L Z . (V $intersect (EX Z)));
let EU V W = LFP (L Z . (W $union (V $intersect (EX Z))))
let EF V   = EU Universe V;