let randoms = iterate nextRand 17489
  where nextRand n = % (+ (* MUL n) INC) MOD
    where MUL = 25173,
          INC = 13849,
          MOD = 65536
    end
  end;
