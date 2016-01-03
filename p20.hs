module Main where

import Debug.Trace

factorialdigitsum :: Integer -> Integer
factorialdigitsum = count . fact
  where count v
            | v < 10 = v
            | otherwise = let (q, r) = v `quotRem` 10 in r + count q
        fact 1 = 1
        fact m = m * fact (m - 1)

main = print $ factorialdigitsum 100
