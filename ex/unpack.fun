let unpack n f a = unpack_1 n f (reverse (take n a))
  where unpack_1 n f a = if (== n 0) f
                         (unpack_1 (- n 1) f (cdr a) (car a))
  end;
