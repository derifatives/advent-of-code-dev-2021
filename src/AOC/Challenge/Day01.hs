-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day01 (
  day01a
  , day01b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split (splitOn)
import Data.Sort (sortBy)

parser :: String -> Maybe [Int]
parser = Just . map (sum . map read) . splitOn [""] . lines 

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . maximum 
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . take 3 . sortBy (flip compare)
    }
