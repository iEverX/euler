module Main where

import Primes

sumPrimes n = sum $ takeWhile (<=n) primes

main = print $ sumPrimes 2000000
