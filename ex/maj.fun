let maj A =
  let _ = L l i . ith i l,
      # = len in
letrec maj1 A N k j c  = if (j `<` N)
                    (if (== (A `_` j) (A `_` k))
                      (maj1 A N k (j `+` 1) (c `+` 1))
                     (if (== c 0)
                       (maj1 A N j (j `+` 1) 1)
                      (maj1 A N k (j `+` 1) (c `-` 1))))
                   [ k : c ] in
let check A N k c =
(
  let occurs A x = #[ a | a <- A; == a x ] in
  if (> c 0)
    (if (> (occurs A (A `_` k)) (div N 2)) [ A `_` k ] [])
  []
)
in
let r = maj1 A (#A) 0 1 1 in
  check A (#A) (car r) (cdr r);