module Main where

factors :: Int -> [Int]
factors n = filter (\x -> n `rem` x == 0) (takeWhile (\x -> x * x <= n) [1..])

divisors :: Int -> Int -> Bool
divisors target num = fix + 2 * (length fns) > target
    where fns = factors num
          v = last fns
          fix = if v * v == num then 1 else 0

-- only worked for n greater than 2
over :: Int -> Int
over n = head $ filter (divisors n) ts
    where ts = 1:3:zipWith (+) (tail ts) [3..]

main = print $ over 500
