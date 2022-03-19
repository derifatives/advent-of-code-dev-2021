-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 6.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day06 (
  day06a
  , day06b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)

import Data.List.Split(splitOn)
import qualified Data.Map as Map

type CountMap a = Map.Map a Integer
type FishMap = CountMap Integer

insert :: Ord a => (CountMap a) -> a -> (CountMap a)
insert cm a = Map.insertWith (+) a 1 cm

parser :: String -> Maybe FishMap
parser input = 
  Just $ foldr (flip insert) Map.empty (map read (splitOn "," input))

runOneDay :: FishMap -> FishMap
runOneDay fm = foldr (\k -> Map.insert k (newCount k)) Map.empty [0..8]
  where lk k = Map.findWithDefault 0 k fm
        newCount k
          | k <= 5 || k == 7 = lk (k + 1)
          | k == 6           = (lk 0) + (lk 7)
          | k == 8           = lk 0
          | otherwise        = error "Unexpected key."

day06a :: FishMap :~> Integer
day06a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . Map.elems . (!! 80) . iterate runOneDay 
    }

day06b :: FishMap :~> Integer
day06b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . sum . Map.elems . (!! 256) . iterate runOneDay 
    }
