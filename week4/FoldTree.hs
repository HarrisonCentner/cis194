{-# OPTIONS_GHC -Wall #-}

main :: IO ()

data Tree a  =  Leaf
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
getHeight :: Tree a -> Integer
setHeight :: Tree a -> Tree a
appendBalanced :: a -> Tree a -> Tree a
giveType :: String -> String

-- TODO: figure out a better way to do this other than completely
-- resetting all the tree heights every time. Maybe only reset along 
-- the inserted path (could use a fold)

-- Leaves have height -1 so that we do not need an extra case
getHeight Leaf = -1
getHeight (Node _ left _ right) = 1 + max (getHeight left) (getHeight right)

setHeight Leaf = Leaf
setHeight n@(Node _ left x right) = 
  Node (getHeight n) (setHeight left) x (setHeight right)

--  empty tree
appendBalanced x Leaf = Node 0 Leaf x Leaf
-- has empty left tree
appendBalanced x (Node _ Leaf y right) = 
  Node 0 (Node 0 Leaf x Leaf) y right
-- has empty right tree
appendBalanced x (Node _ left y Leaf) = 
  Node 0 left y (Node 0 Leaf x Leaf)
-- neither subtree is empty
appendBalanced x (Node _ lhs@(Node hLeft _ _ _) y rhs@(Node hRight _ _ _)) 
  | hLeft < hRight = Node 0 (appendBalanced x lhs) y rhs
  | hLeft > hRight = Node 0 lhs y (appendBalanced x rhs)
  | otherwise      = Node 0 lhs y (appendBalanced x rhs)

foldTree = foldr (\x tree -> setHeight $ appendBalanced x tree) Leaf 

-- thwarts compiler errors related to printing
giveType arr = arr

main = do
    ints <- readLn
    print $ foldTree $ giveType ints


--
--                  J 1
--           H 1           I 1
--       F 0            G 0   E 0
--
--
--
--
--
--
--
--
--
--
--
