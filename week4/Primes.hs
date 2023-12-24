{-# OPTIONS_GHC -Wall #-}

import Data.List ((\\))

main :: IO ()

-- Finds all primes using the Sieve of Sundaram
sieveSundaram  :: Integer -> [Integer]
cartProd :: [a] -> [b] -> [(a,b)]

cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- get all pairs (i,j) in [1..n] with i < j
-- sieveSundaram n = filter (\(i,j) -> i <= j) (cartProd [1..n] [1..n])
sieveSundaram n = 
  2 : map (\k -> 2*k+1) 
  (
    [1..n] \\ 
    (map (\(i,j) -> i + j + 2 * i * j) (cartProd [1..div n 2] [1..div n 2]))
  )

-- You actually don't event need to do that



main = do
    n <- readLn
    print $ sieveSundaram n


