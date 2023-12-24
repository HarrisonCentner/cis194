
factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n-1)

factorialList :: Int -> [Int]
factorialList 0 = [0]
factorialList 1 = [1]
factorialList n = n :  factorialList (factorial n)
 
main = putStrLn "factorialList 16"

