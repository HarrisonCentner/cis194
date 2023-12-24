{-# OPTIONS_GHC -Wall #-} 

-- Exercise 1
fib :: Integer -> Integer
fibs1 :: [Integer]

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]    
naturals :: [Integer]
naturals = [0..]
fibs2 = map fst $ scanl (\(n,m) _ -> (m, n+m)) (0,1) naturals

-- Exercise 3
data Stream a = Cons a (Stream a)
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = [x] ++ streamToList xs

instance (Show a) => Show (Stream a) where
  show x = show (take 20 (streamToList x))

-- Exercise 4 
streamRepeat :: a -> Stream a
streamMap :: (a -> b) -> Stream a ->  Stream b
streamFromSeed :: (a -> a) -> a -> Stream a

streamRepeat x = Cons x (streamRepeat x)
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
-- Exercise 5
nats :: Stream Integer
ruler :: Stream Integer
rulerFunc :: Integer -> Integer

nats = streamFromSeed (1+) 0

rulerFunc 0 = 0
rulerFunc n = fromIntegral $ length $ 
    takeWhile (\m -> mod m 2 == 0) $ iterate (\x -> div x 2) n

ruler = streamMap rulerFunc nats

-- Exercise 6
-- TODO: Fibonacci via Generating Functions
-- O(n)


-- Exercise 6
-- TODO: Fibonacci via Matrix Exponentiation 
-- O(log n)
