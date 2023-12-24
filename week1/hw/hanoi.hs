{-# OPTIONS_GHC -Wall #-}

main :: IO ()
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- Solve Hanoi by:
--- move n-1 discs from a to c using b as temp storage
--- move the top disc from a to b
--- move n-1 discs from c to b using a as temporary storage
hanoi 0 _ _ _ = []
hanoi 1 src dst _ = (src, dst) : []
hanoi n src dst tmp = hanoi (n-1) src tmp dst ++ 
  ((src, dst) : hanoi (n-1) tmp dst src)


main = do
  n <- readLn
  a <- readLn
  b <- readLn
  c <- readLn
  print n
  print (a : b : c : [])
  print (hanoi n a b c)
