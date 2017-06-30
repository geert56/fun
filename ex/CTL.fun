/* CTL Model-Checking. Copyright (c) 1995-1996 G. Janssen */

/* Must specify a next-state function: R : Q -> 2^Q */
/*let R v = ith v [ [0,1,2], [0,2,3], [1,2,3], [0] ];*/
/*let R v = ith v [ [1], [2], [0,3], [] ];*/
/*let R v = ith v [ [0,1], [2], [3], [3] ];*/
let R v = ith v [ [1], [3], [0], [3] ];

/* Complete set of CTL operators: { EX, EU, EG } */

/* Returns True when there exists a successor of v for which the predicate p
   holds.
*/
let EX p v = any p (R v);

let EG p v = EG* p [v] []
  where EG* p s path =
    if (null s) False
      (let v' = car s, rest = cdr s in
        if (member v' path) True
        (|| (&& (p v') (EG* p (R v') [v':path]))
            (EG* p rest path)))
  end;

let EU p q v = EU* p q [v] []
  where EU* p q s path =
    if (null s) False
      (let v' = car s, rest = cdr s in
        if (member v' path) False /* put True here then weak-until */
        (|| (|| (q v') (&& (p v') (EU* p q (R v') [v':path])))
            (EU* p q rest path)))
  end;

/* Derived operators (abbreviations): */
let dual_1 op a   = not . (op (not . a));
let dual_2 op a b = not . (op (not . a) (not . b));
let All  v = True;
let None v = False;

let EF = EU All;
let EUw p q = || (EU p q) (EG p);

let AX = dual_1 EX;
let AG = dual_1 EF;
let AF = dual_1 EG;
let AU  p q = dual_2 EUw q (|| p q);
let AUw p q = dual_2 EU  q (|| p q);

/*
let AU p q = L v.not (|| (EU (not.q) (L v.(&& ((not.p) v) ((not.q) v))) v)
                         (EG (not.q) v));
let AF p   = AU (L v.True) p;
*/