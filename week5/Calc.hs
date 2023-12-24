{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-} 

module Calc where
import ExprT
import Parser
import qualified Data.Map as M

eval :: ExprT -> Integer
evalStr :: String -> Maybe Integer
reify :: ExprT -> ExprT

reify = id

-- Exercise 1
eval (ExprT.Lit i) = i
eval (ExprT.Add e1 e2) = (eval e1) + (eval e2)
eval (ExprT.Mul e1 e2) = (eval e1) * (eval e2)

-- Exercise 2
evalStr s = 
    case parseExp ExprT.Lit ExprT.Add ExprT.Mul s of
      (Just a)  -> Just (eval a)
      (Nothing) -> Nothing
    
-- Exercise 3
class Expr exp where
  mul :: exp -> exp -> exp 
  add :: exp -> exp -> exp
  lit :: Integer -> exp

-- implement ExprT type of Expr
instance Expr ExprT where
  mul x y = ExprT.Mul x y
  add x y = ExprT.Add x y
  lit x   = ExprT.Lit x


-- Exercise 4 
instance Expr Integer where
  mul x y = x * y
  add x y = x + y
  lit x   = x

instance Expr Bool where
  mul x y = x && y
  add x y = x || y
  lit x   = x > 0

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr MinMax where
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)
  lit x   = MinMax x

instance Expr Mod7 where
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x*y) 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x+y) 7)
  lit x   = Mod7 (mod x 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


-- Exercise 5
compile :: String -> Maybe Program
reify :: Maybe Program -> Maybe Program

reify = id

instance Expr Program where
  mul lhs rhs = lhs ++ add : Mul : []
  add lhs rhs = lhs ++ add : Add : []
  lit int     = [PushI int]

-- TODO: finish this
compile s = execute maybe (parseExp lit add mul s) Nothing

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Var String 
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  mul x y = Calc.Mul x y
  add x y = Calc.Add x y
  lit x   = Calc.Lit x

instance HasVars VarExprT where
  var x = Calc.Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = M.lookup x

-- Uses the functor fmap with is abbreviated by <$>
-- then we use the applicative functor <*>
-- See: https://stackoverflow.com/questions/22268226/multiplying-the-value-within-two-maybe-monads
instance Expr (M.Map String Integer -> Maybe Integer) where
     lit x   = (\_ -> Just x)
     mul lmap rmap = (\s -> (*) <$> (lmap s) <*> (rmap s))
     add lmap rmap = (\s -> (+) <$> (lmap s) <*> (rmap s))

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer

withVars vs exp = exp $ M.fromList vs








