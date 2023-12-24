{-# OPTIONS_GHC -Wall #-}

main :: IO ()
toDigits    ::  Integer  -> [Integer]
toDigitsRev ::  Integer  -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherHelper :: [Integer] -> [Integer]
sumDigits :: [Integer] -> Integer
validate :: Integer -> Bool

toDigits n
  | n <= 0 = []
  | otherwise = toDigits(div n 10) ++ [mod n 10]

toDigitsRev arr = reverse (toDigits arr)

doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:[]) = x : []
doubleEveryOtherHelper (x:xs:xss) = x : 2 * xs : doubleEveryOtherHelper xss

doubleEveryOther x = reverse (doubleEveryOtherHelper (reverse x))

sumDigits [] = 0
sumDigits (x:xs) = (div x 10) + (mod x 10) + sumDigits xs

validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0



main = do
  card <- readLn
  print (toDigits card)
  print (toDigitsRev card)
  let out = doubleEveryOther (toDigits card)
  print out
  print (sumDigits out)
  print (validate card)
