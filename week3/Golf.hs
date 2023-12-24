-- Submit File
{-# OPTIONS_GHC -Wall #-}

import Data.List (zip3, sort, group, transpose)

main :: IO ()

-- for skips
z :: [a] -> [(Integer, a)]
d :: (Integer, [a]) -> [a]
skips :: [Char] -> [[Char]]

-- equivalent to the enumerate method in rust
-- a is list
z a = zip [1..] a

-- removes all elements from a that are not multiples of i
d (i, a) = [ x | (k, x) <- z a, 0 == mod k i] 

-- produces the skips array
skips x = map d (z (replicate (length x) x))


-- for localMaximaa
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:xs) = [ b | (a,b,c) <- zip3 (x : xs) xs (drop 1 xs), 
  a < b && b > c]


-- for Histogram
histogram :: [Integer] -> String
l :: [Integer] -> Int
h :: Int -> Int -> String
a :: String -> String

-- computes length - 1
l m = length m - 1

-- builds the star map
h m n = replicate n '*' ++ replicate m ' '
-- appends an endline
a s = s ++ "\n"
-- first we grab the count of each digit
-- then make a list of * followed by spaces
-- then we cutoff any extra remaining spaces
-- then we transpose
-- join with the base of the list
histogram arr = 
  let len = map l (group $ sort $ [0..9] ++ arr) in
      let m = 1 + maximum len in 
        unwords $ 
        reverse $
        map a (
          ["0123456789"] ++ ["=========="]  ++
          (
            transpose $ map (take m) (map (h m) len)
          )
        )


main = do
    thing <- readLn
    print $ skips thing
    ints <- readLn
    print $ localMaxima ints
    ints <- readLn
    putStr $ histogram ints



