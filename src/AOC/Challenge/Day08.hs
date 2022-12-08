-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day08 (
  day08a
  , day08b
  ) where

import AOC.Solver ((:~>)(MkSol), sParse, sShow, sSolve)
import Data.Array.Unboxed
import Data.Char (digitToInt)
import Linear.V2
import qualified Data.Set as S

type Ind = V2 Int
type Forest = UArray Ind Int
type Visible = S.Set Ind

parser :: String -> Maybe Forest
parser s =
  let lls = lines s
      limits = (V2 0 0, V2 (length lls - 1) (length (lls !! 0) - 1))
  in Just $ listArray limits (concat (map (map digitToInt) lls))

addRayToVisible :: Forest -> Ind -> Int -> Ind -> Visible -> Visible
addRayToVisible f o heightToBeat p visible =
  if not $ inRange (bounds f) p
  then visible
  else
    let height = f ! p
    in
      if height > heightToBeat
      then addRayToVisible f o height (p + o) (S.insert p visible)
      else addRayToVisible f o heightToBeat (p + o) visible


addDirectionToVisible :: Forest -> (Ind, [Ind]) -> Visible -> Visible
addDirectionToVisible forest (offset, starts) visible =
  foldr (addRayToVisible forest offset (-1)) visible starts
  
offsets :: [Ind]
offsets = [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

findVisible :: Forest -> Visible
findVisible forest =
  let (_, V2 maxRow maxCol) = bounds forest
      borders = [[V2 i 0 | i <- [0..maxRow]],
                 [V2 i maxCol | i <- [0..maxRow]],
                 [V2 0 i | i <- [0..maxCol]],
                 [V2 maxRow i | i <- [0..maxCol]]]
      adders = map (addDirectionToVisible forest) (zip offsets borders)
  in (foldr (.) id adders) S.empty

rayScenicScore :: Forest -> Int -> Ind -> Ind -> Int
rayScenicScore forest height position offset
  | not $ inRange (bounds forest) position = 0
  | forest ! position >= height = 1
  | otherwise = 1 + rayScenicScore forest height (position + offset) offset

treeScenicScore :: Forest -> Ind -> Int
treeScenicScore forest position =
  let height = forest ! position
  in product $ map (\o -> rayScenicScore forest height (position+o) o) offsets

forestScenicScores :: Forest -> [Int]
forestScenicScores f = map (treeScenicScore f) (indices f)

day08a :: Forest :~> Int
day08a = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . S.size . findVisible
    }

day08b :: Forest :~> Int
day08b = MkSol
    { sParse = parser
    , sShow  = show
    , sSolve = Just . maximum . forestScenicScores
    }
