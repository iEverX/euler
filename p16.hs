module Main where

import Debug.Trace

powerDigitSum :: Integer -> Integer
powerDigitSum n = count (2 ^ n)
  where count v
            | v < 10 = v
            | otherwise = let (q, r) = v `quotRem` 10 in r + count q

main = print $ powerDigitSum 1000
