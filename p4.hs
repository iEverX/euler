module Main where

isPalindromic :: Int -> Bool
isPalindromic n = n == back n

back :: Int -> Int
back = b 0
    where b acc 0 = acc
          b acc n = b (10 * acc + r) q
            where (q, r) = n `quotRem` 10

find :: Int -> Int -> Int
find big small = maximum . filter isPalindromic . map mul $ [(x,y)|x<-r,y<-r,x>=y]
    where mul (x,y) = x * y
          r = [big,big-1..small]

main = print $ find 999 100
