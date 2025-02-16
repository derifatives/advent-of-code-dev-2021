-- Module      : AOC.Challenge.Day05
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 5.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day05 (
  day05a
  , day05b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)

import Data.List.Split(splitOn)
import qualified Data.Map as Map

type Pos = (Int, Int)
type Vent = (Pos, Pos)

type CountMap a = Map.Map a Int
type VentMap = CountMap Pos

insert :: Ord a => a -> (CountMap a) -> (CountMap a)
insert a = Map.insertWith (+) a 1

toTuple :: [a] -> (a, a)
toTuple [a1, a2] = (a1, a2)
toTuple _ = error "Not a 2-list"

parser :: String -> Maybe ([Vent])
parser = Just . map (readVent . splitOn "->") . lines
  where readVent = toTuple . map parsePos
        parsePos = toTuple . map read . splitOn ","
    
notDiagonal :: Vent -> Bool
notDiagonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

ventToPositions :: Vent -> [Pos]
ventToPositions ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) (range y1 y2)
  | y1 == y2 = map (\x -> (x, y1)) (range x1 x2)
  | otherwise = zip (range x1 x2) (range y1 y2)
  where range a b = if a < b then [a..b] else reverse [b..a]

addPositions :: [Pos] -> VentMap -> VentMap
addPositions ps vm = foldr insert vm ps

buildVentMap :: [Vent] -> VentMap
buildVentMap vents = foldr addPositions Map.empty (map ventToPositions vents)

dangerousPoints :: VentMap -> [Pos]
dangerousPoints = map fst . filter ((>= 2) . snd) . (Map.assocs)

day05a :: [Vent] :~> Int
day05a = MkSol
    { sParse = (fmap (filter notDiagonal)) . parser
    , sShow  = show
    , sSolve = (\ps -> Just (length ps)) . dangerousPoints . buildVentMap 
    }

day05b :: [Vent] :~> Int
day05b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = (\ps -> Just (length ps)) . dangerousPoints . buildVentMap
    }
