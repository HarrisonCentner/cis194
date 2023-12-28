{-# OPTIONS_GHC -Wall #-}

module JoinList where
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlbToList :: JoinListBasic a -> [a]
jlbToList Empty               = []
jlbToList (Single _ a)        = [a]
jlbToList (Append _ jl1 jl2)  = jlbToList jl1 ++ jlbToList jl2

-- Exercise 1
tag :: Monoid m => JoinList m a -> m
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- get the annotation of roots of jl1 and jl2 
(+++) jl1 jl2 = Append ((tag jl1) <> (tag jl2)) jl1 jl2

-- Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
dropJ  :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ  :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
getSizeTag ::  (Sized b, Monoid b) => JoinList b a -> Int

getSizeTag = getSize . size . tag

indexJ _ Empty = Nothing
indexJ i (Single _ x) 
  | i == 0    = Just x
  | otherwise = Nothing
indexJ i app@(Append _ jl1 jl2) 
  | i >= (getSizeTag app) = Nothing
  | i < (getSizeTag jl1)  = indexJ i jl1
  | otherwise             = indexJ (i-(getSizeTag jl1)) jl2

-- program dropJ so that the diagram below commutes
--
--    JoinList --->  List
-- dropJ |            | drop
--       v            v 
--    JoinList --->  List
--           jlToList
dropJ _ Empty = Empty
dropJ i s@(Single _ _)
  | i <= 0    = s
  | otherwise = Empty
dropJ i app@(Append _ jl1 jl2) 
  | i <= 0                  = app
  | i >= (getSizeTag app)   = Empty
  | i < (getSizeTag jl1)    = (dropJ i jl1) +++ jl2
  | otherwise               = (dropJ (i - getSizeTag jl1) jl2)
   
-- program takeJ and take commute over JoinList and List
takeJ _ Empty = Empty
takeJ i s@(Single _ _)
  | i <= 0    = Empty
  | otherwise = s
takeJ i app@(Append _ jl1 jl2) 
  | i <= 0                  = Empty
  | i >= (getSizeTag app)   = app
  | i <= (getSizeTag jl1)   = takeJ i jl1
  | otherwise               = jl1 +++ 
                (takeJ (i - getSizeTag jl1) jl2)


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString    = unwords . jlbToList
  fromString  = -- TODO:
  line        = indexJ
  replaceLine = 
  numLines    = size . snd . tag
  value       = size . fst . tag
    



