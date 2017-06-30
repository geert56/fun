/* Definitely not complete!!! */

/* STOP process */
let STOP = L x.BLEEP;

/* RUN process */
let RUN = L x. RUN;

/* Prefix */
let \-> x P = L y. if (== y x) P BLEEP;

/* Binary guarded choice */
let \| p P q Q = L x. if (== p q) (error "Invalid guards in \\|")
                     (if (== x p) P
                     (if (== x q) Q
                      BLEEP));

let \|| p P q Q r R = L x.
                     (if (== x p) P
                     (if (== x q) Q
                     (if (== x r) R
                      BLEEP)));

/* Returns which of the symbols in A are valid prefixes for process P */
letrec menu A P = if (|| (null A) (== P BLEEP)) []
                 (if (== BLEEP (P (car A))) (menu (cdr A) P)
                 [ car A : menu (cdr A) P ]);

letrec VMS = coin $\-> (choc $\-> VMS);

letrec VMC = \| in2p (\| large VMC small (\-> out1p VMC))
                in1p (\| small VMC in1p (\| large VMC in1p STOP));

let DD = (\| setorange O setlemon LL)
         where O = \|| orange O setlemon LL setorange O,
               LL = \|| lemon LL setorange O setlemon LL end;

/* Simulates process P on trace t */
letrec simu P t = if (null t) []
                 (if (== P BLEEP) t
                 (let t1 = car t, r = P t1 in
                    if (== r BLEEP) t
                      (simu r (cdr t))));
