module Main where

import Data.List

solve :: Int -> Int
solve n = sum $ takeWhile (<n) $ filter even fib
    where fib = 1:2:zipWith (+) fib (tail fib)

main = print $ solve 4000000
