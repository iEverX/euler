module Main where

findPrime :: Int -> Int
findPrime n = find [2] 1 3
    where find ps m p
            | m == n = head ps
            | all (\x -> p `rem` x /= 0) . takeWhile (\x -> x ^ 2 <= p) . reverse $ ps = find (p:ps) (m+1) (p+2)
            | otherwise = find ps m (p+2)

main = print $ findPrime 10001
