module Main where

diff :: Int -> Int
diff n = (sum r) ^ 2 - sum (map (^2) r)
    where r = [1..n]

main = print $ diff 100
