/* Least and Greatest Fixed-point functors.
   Depend on global definition for Universe of set Q.
*/

/* Least fixed-point of f : 2^Q -> 2^Q */
let LFP f = LFP* f []
  where LFP* f Zi =
    let Ziplus1 = f Zi in
      if (equal Ziplus1 Zi)
          Zi
        (LFP* f Ziplus1)
  end;

/* Greatest fixed-point: */
let GFP f = GFP* f Universe
  where GFP* f Zi =
    let Ziplus1 = f Zi in
      if (equal Ziplus1 Zi)
          Zi
        (GFP* f Ziplus1)
  end;
