-- |
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

insert :: Ord a => (CountMap a) -> a -> (CountMap a)
insert cm a = Map.insertWith (+) a 1 cm

parser :: String -> Maybe ([Vent])
parser input =
  let lls = lines input
      poslines = map (splitOn " -> ") lls
      vents = map readVent poslines
  in Just vents
  where readVent posline =
          let poses = map parsePos posline
          in (head poses, head (tail poses))
          
parsePos :: String -> Pos
parsePos p =
  let ns = map read (splitOn "," p)
  in (head ns, head (tail ns))

notDiagonal :: Vent -> Bool
notDiagonal ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

ventToPositions :: Vent -> [Pos]
ventToPositions ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) (range y1 y2)
  | y1 == y2 = map (\x -> (x, y1)) (range x1 x2)
  | otherwise = zip (range x1 x2) (range y1 y2)
  where range a b = if a < b then [a..b] else reverse [b..a]

addPositions :: [Pos] -> VentMap -> VentMap
addPositions ps vm = foldr (flip insert) vm ps

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
