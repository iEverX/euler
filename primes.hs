module Primes where

-- code comes from https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes = 2:3:([5,7..] `minus` composites)
  where composites = union [multiples p | p <- tail primes]
        multiples n = map (n*) [n,n+2..]
        m = multiples

        minus (x:xs) (y:ys)
          | x < y = x : minus xs (y:ys)
          | x == y = minus xs ys
          | x > y = minus (x:xs) ys

        union = foldr merge []
          where merge (x:xs) ys = x : merge' xs ys
                merge' (x:xs) (y:ys)
                  | x < y = x : merge' xs (y:ys)
                  | x == y = x : merge' xs ys
                  | x > y = y : merge' (x:xs) ys
