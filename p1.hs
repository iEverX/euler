module Main where

solve :: Int -> Int
solve n = cal(3) + cal(5) - cal(15)
  where cal x = let m = (n - 1) `quot` x in (1 + m) * m * x `quot` 2

main = print $ solve 1000
