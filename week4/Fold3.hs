{-# OPTIONS_GHC -Wall #-}

main :: IO ()

-- Exercise 1
xor  :: [Bool] -> Bool
map' :: (a -> b) -> [a] -> [b]
double :: Integer -> Integer

xor = foldr (\y x -> (x && (not y)) || ((not x) && y)) False 
double x = 2 * x

map' f arr = foldr (\x t -> f x : t) [] arr


main = do
    bools <- readLn
    print $ xor bools
    ints <- readLn
    print $ map' double ints


