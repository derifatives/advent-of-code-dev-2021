-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 4.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day04 (
  day04a
  , day04b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)

type TwoRanges = ((Int, Int), (Int, Int))

firstTwo :: [x] -> (x, x)
firstTwo xs = (xs !! 0, xs !! 1)

parser :: String -> Maybe [TwoRanges]
parser = Just . map (firstTwo . map parseRange . splitOn ",") . lines
  where parseRange = firstTwo . map read . splitOn "-"
          
oneContainsOther :: TwoRanges -> Bool
oneContainsOther ((b1, t1), (b2, t2)) =
  (b1 <= b2 && t2 <= t1) || (b2 <= b1 && t1 <= t2)

overlaps :: TwoRanges -> Bool
overlaps ((b1, t1), (b2, t2)) = (max b1 b2) <= (min t1 t2)
  
day04a :: [TwoRanges] :~> Int
day04a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map (fromEnum . oneContainsOther)
    }

day04b :: [TwoRanges] :~> Int
day04b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . map (fromEnum . overlaps)
    }
