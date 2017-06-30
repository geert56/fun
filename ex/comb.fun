let then = if;
let else = I;

letrec subs l = (null l) $then [[]]
                         $else (subs (cdr l) $++ map [car l:] (subs (cdr l)));

letrec scan f a xs = [ a : scan' f a xs ]
  where scan' f a xs = (null xs) $then []
                                 $else scan f (f a (car xs)) (cdr xs)
  end;
