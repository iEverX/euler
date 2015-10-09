module Main where

import Data.List

primes :: Int -> [Int]
primes = prime [] 2
    where prime ps p n
            | n == 1 = ps
            | n `rem` p == 0 = prime (p:ps) p (n `quot` p)
            | otherwise = prime ps (p + 1) n

count :: [Int] -> [(Int, Int)]
count = map c . group
    where c (xs@(x:_)) = (x, length xs)

merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
merge as bs = map r $ groupBy eq xs
    where eq x y = fst x == fst y
          xs = sort $ as ++ bs
          r (xs@((x,_):_))= (x, maximum . map snd $ xs)

mul :: [(Int, Int)] -> Int
mul = product . map pow
    where pow (x,y) = x ^ y

solve :: Int -> Int
solve n = mul . foldr1 merge . map (count . primes) $ [2..n]

main = print $ solve 20
