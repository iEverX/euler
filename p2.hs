module Main where

import Data.List

fib = 1:2:zipWith (+) fib (tail fib)

solve :: Int -> Int
solve n = sum $ takeWhile (\x -> x < n) $ filter (even) fib

main = print $ solve 4000000
