{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where
import Employee 
import Data.Tree 

   
-- Exercise 1
glCons  :: Employee  -> GuestList -> GuestList
moreFun :: GuestList -> GuestList -> GuestList

glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)
moreFun = max

instance Semigroup GuestList where
  (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend = (<>)

-- Exercise 2
treeFold :: (a -> [b] -> b) -> Tree a -> b

treeFold f (Node {subForest = [], rootLabel = a}) = f a []
treeFold f (Node {subForest = subTree, rootLabel = root})
  = f root (map (treeFold f) subTree)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
dropBoss  :: GuestList -> GuestList
foldGL    :: [GuestList] -> GuestList

dropBoss (GL xs fun) = 
  GL (drop 1 xs) (if null xs then 0 else fun - (empFun $ head xs))

foldGL = foldr (<>) mempty

nextLevel boss []   = (GL [boss] (empFun boss),  mempty)
nextLevel boss list = let (yesSubBoss, noSubBoss) = unzip list in
  let noSubFolded = foldGL noSubBoss in
  (glCons boss 
  (max noSubFolded (foldr (\x acc->(dropBoss x) <> acc) mempty yesSubBoss)),
        max (foldGL yesSubBoss) noSubFolded) 

-- Exercise 4
maxFun    :: Tree Employee -> GuestList
maxFun = (uncurry max) . treeFold nextLevel

-- Exercise 5 
main :: IO ()
getFun :: GuestList -> Fun
getEmployees :: GuestList -> [Employee]


getFun (GL _ fun) = fun
getEmployees (GL xs _) = xs


main = do
  text <- readFile "company.txt"  
  let out = maxFun $ read text
  putStrLn ("Total fun: " ++ (show $ getFun out))
  mapM_ (putStrLn . empName) (getEmployees out)

      



