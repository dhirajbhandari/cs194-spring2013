{-# OPTIONS_GHC -Wall #-}

module Hw04 where
  
import Data.List

fun1 :: [Integer] -> Integer
 -- fun1 [] = 1
{- fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs
-}  
fun1 xs = foldl (\x t -> (x - 2) * t) 1 (filter even xs)
-- fun1 xs = foldl (+) (filter even xs) 
{-
fInt :: Integral a => (a -> Bool) -> [a] -> [a]
fInt = 
  let 
    multiplier = (\x t = (x - 2) * t)
    multiplier x t = (x - 2) * t
   in
    foldl multiplier 1 . filter even
-
-}
 -- multiplier :: a -> a -> a
 -- let multiplier = (\x t -> (x - 2) * t)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

