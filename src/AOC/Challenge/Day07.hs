-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day07 (
  day07a
  , day07b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.List.Split(splitOn)

parser :: String -> Maybe [Int]
parser = Just . map read . splitOn ","

totalDistance :: (Int -> Int) -> [Int] -> Int -> Int
totalDistance f as c = sum $ map (\a -> f $ abs(a - c)) as

minTotalDistance :: (Int -> Int) -> [Int] -> Int
minTotalDistance f as = minimum $ map (totalDistance f as) [(minimum as)..(maximum as)]
                                                                         
day07a :: [Int] :~> Int
day07a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minTotalDistance id
    }

day07b :: [Int] :~> Int
day07b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . minTotalDistance (\n -> (n * (n+1) `div` 2))
    }
