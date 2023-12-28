{-# OPTIONS_GHC -Wall #-}

module Scrabble where
import Data.Char (toUpper)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty  = Score 0
  mappend = (<>)

score :: Char -> Score
scoreString :: String -> Score

score x
  | elem y "AEILNORSTU" = 1
  | elem y "DG"         = 2
  | elem y "BCMP"       = 3
  | elem y "FHVWY"      = 4
  | elem y "K"          = 5
  | elem y "JX"         = 8
  | elem y "QZ"         = 10
  | otherwise           = 0
  where y = toUpper x

scoreString = sum . map score 


