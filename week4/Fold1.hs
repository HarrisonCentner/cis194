{-# OPTIONS_GHC -Wall #-}

main :: IO ()

-- Exercise 1
fun1  :: [Integer] -> Integer
fun2  ::  Integer  -> Integer
fun1' :: [Integer] -> Integer
fun2' ::  Integer  -> Integer
collatz ::  Integer  -> Integer

fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- this is the sum of all odd elements in the Collatz orbit of n
fun2 1 = 0
fun2 n
  | even n   = n + fun2 (n `div` 2) 
  | otherwise = fun2 (3 * n + 1)


fun1' = product . map (\x -> x - 2) . filter even

collatz n
    | even n    = div n 2
    | otherwise = 3 * n + 1
fun2' = sum . filter even . takeWhile (1/=) . iterate collatz



main = do
    ints <- readLn
    print $ fun1 ints == fun1' ints
    print $ fun1 ints
    print $ fun1' ints
    num <- readLn
    print $ fun2 num == fun2' num
    print $ fun2 num
    print $ fun2' num


