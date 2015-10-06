module Main where

import Control.Monad

maxPrimeFactor :: Int -> Int
maxPrimeFactor = prime 2
    where prime p n
              | n == 1 = p
              | n `rem` p == 0 = prime p (n `quot` p)
              | otherwise = prime (p + 1) n

debugIo :: (Int -> Int) -> IO ()
debugIo f = do
            n <- getLine
            print $ f (read n :: Int)

-- main = forever $ debugIo maxPrimeFactor
main = print $ maxPrimeFactor 600851475143
