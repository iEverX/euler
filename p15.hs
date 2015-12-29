module Main where

import Data.Array
import Debug.Trace

path :: Int -> Int -> Integer
path p q = fp p q
  where fp _ 0 = 1
        fp 0 _ = 1
        fp m n = a ! (m - 1, n) + a ! (m, n - 1)
        a = listArray bounds [fp i j | (i, j) <- range bounds]
        bounds = ((0, 0), (p, q))

main = print $ path 20 20
