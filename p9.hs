module Main where

triple n = case [x*y*z|x<-[1..n `quot` 2], y<-[1.. n `quot` 2], let z = n - x - y, x <= y, y <= z, x^2 + y^2 == z^2] of
             [] -> 0
             (x:_) -> x

main = print $ triple 1000
